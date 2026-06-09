/*******************************************************************************
*                                                                              *
*                     Terminal Library Support for Pascaline                   *
*                                                                              *
*                              Created 2024                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Conversion helpers for the Pascaline terminal wrappers: event record         *
* translation. The straight parameter wrappers live in terminal_wrapper.c.     *
*                                                                              *
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <terminal.h>
#include <support.h>

/********************************************************************************

Convert C event record to Pascaline event record

The Petit-Ami event record (ami_evtrec) uses C int (4 byte) fields and an
anonymous union. The Pascaline 'evtrec' variant record uses 8 byte integers and
a different field layout, determined from the generated DWARF:

    winid   @ 0   (8 byte integer)
    handled @ 8   (1 byte boolean)
    etype   @ 9   (1 byte tag; enums are byte sized)
    variant fields from offset 12, laid out in REVERSE declaration order, each
    8 bytes (echar is a single byte).

The named event codes (etchar..etmenus) are in the same order in both the C and
Pascaline enumerations, so the tag is copied directly. The reserved codes
(etsys..etuser, >= 0x1000) are never delivered as events.

********************************************************************************/

#define PD 12 /* base of variant data */

void cevt2pascaline(
    /** C event record */         ami_evtrec* ce,
    /** Pascaline event record */ char*       pe
)

{

    PEVTL(pe, PA_EVT_WINID)   = ce->winid;   /* window id */
    PEVTC(pe, PA_EVT_HANDLED) = ce->handled; /* handled flag */
    PEVTC(pe, PA_EVT_ETYPE)   = ce->etype;   /* event type tag */
    switch (ce->etype) {

        case ami_etchar:   PEVTC(pe, PD)    = ce->echar;  break;
        case ami_ettim:    PEVTL(pe, PD)    = ce->timnum; break;
        case ami_etmoumov: PEVTL(pe, PD)    = ce->moupy;
                           PEVTL(pe, PD+8)  = ce->moupx;
                           PEVTL(pe, PD+16) = ce->mmoun;  break;
        case ami_etmouba:  PEVTL(pe, PD)    = ce->amoubn;
                           PEVTL(pe, PD+8)  = ce->amoun;  break;
        case ami_etmoubd:  PEVTL(pe, PD)    = ce->dmoubn;
                           PEVTL(pe, PD+8)  = ce->dmoun;  break;
        case ami_etjoyba:  PEVTL(pe, PD)    = ce->ajoybn;
                           PEVTL(pe, PD+8)  = ce->ajoyn;  break;
        case ami_etjoybd:  PEVTL(pe, PD)    = ce->djoybn;
                           PEVTL(pe, PD+8)  = ce->djoyn;  break;
        case ami_etjoymov: PEVTL(pe, PD)    = ce->joyp6;
                           PEVTL(pe, PD+8)  = ce->joyp5;
                           PEVTL(pe, PD+16) = ce->joyp4;
                           PEVTL(pe, PD+24) = ce->joypz;
                           PEVTL(pe, PD+32) = ce->joypy;
                           PEVTL(pe, PD+40) = ce->joypx;
                           PEVTL(pe, PD+48) = ce->mjoyn; break;
        case ami_etfun:    PEVTL(pe, PD)    = ce->fkey;   break;
        case ami_etresize: PEVTL(pe, PD)    = ce->rszy;
                           PEVTL(pe, PD+8)  = ce->rszx;   break;
        case ami_etmenus:  PEVTL(pe, PD)    = ce->menuid; break;
        default: break; /* events without parameter data */

    }

}

/********************************************************************************

Event wrappers

procedure event(var f: text; var er: evtrec); and the default form. The file is
ignored by the console terminal model, which operates on global state, but the
input form is routed to the read file for symmetry. The result event record is
converted to Pascaline form.

********************************************************************************/

void wrapper_eventf(
    /** input file */   pfile pfp,
    /** event record */ char* per
)

{

    FILE* f = psystem_libcrdfil(pfp);
    ami_evtrec ce; /* C event record */

    ami_event(f, &ce);
    cevt2pascaline(&ce, per);

}

void wrapper_event(
    /** event record */ char* per
)

{

    ami_evtrec ce; /* C event record */

    ami_event(stdin, &ce);
    cevt2pascaline(&ce, per);

}

/********************************************************************************

Callback thunks

Petit-Ami stores a callback as a single C function pointer and calls it later
(deferred). A Pascaline procedure is two words (code, display), so it cannot be
handed over directly. For each installed callback we build a small machine code
thunk that captures (code, display) and the dispatcher address, and presents the
plain C function pointer Petit-Ami expects. When Petit-Ami calls the thunk, the
thunk tail-calls the dispatcher, which converts arguments and uses pacall() to
enter the Pascaline procedure with the captured display.

The thunks live in a fixed executable pool, so they persist for the life of the
program (top level event handlers and thread bodies have a program-lifetime
display, so this is exactly what is wanted).

********************************************************************************/


/* dispatch an event callback: convert the C event, enter the Pascaline handler,
   then copy the handled flag back */
void evtdispatch(void* code, void* display, ami_evtrec* cer)
{

    char per[80]; /* Pascaline event record buffer */

    cevt2pascaline(cer, per);
    pacall(code, display, per);
    cer->handled = PEVTC(per, PA_EVT_HANDLED);

}

/********************************************************************************

Event override wrappers

The procedure parameter arrives as (code, display) in consecutive registers. The
old handler is returned through an 'out oeh: integer', holding the previous C
handler pointer (the test saves it but does not call it).

********************************************************************************/

void wrapper_eventover(
    /** event code */        int   e,
    /** handler code */      void* eh_code,
    /** handler display */   void* eh_display,
    /** old handler out */   void* oeh
)

{

    ami_pevthan old;

    ami_eventover(e, (ami_pevthan)mkevtthunk(eh_code, eh_display, (void*)evtdispatch), &old);
    *(void**)oeh = (void*)old;

}

void wrapper_eventsover(
    /** handler code */    void* eh_code,
    /** handler display */ void* eh_display,
    /** old handler out */ void* oeh
)

{

    ami_pevthan old;

    ami_eventsover((ami_pevthan)mkevtthunk(eh_code, eh_display, (void*)evtdispatch), &old);
    *(void**)oeh = (void*)old;

}

