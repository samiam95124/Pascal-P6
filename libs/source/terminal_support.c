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
#include <terminal_wrapper.h>

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

Call a Pascaline procedure from C

A Pascaline procedure value is a pair (code address, display pointer). The
display is the frame pointer of the lexically enclosing scope; the procedure's
ENTER instruction uses it (via %rbp) to build its own display. To call such a
procedure from C we set %rbp to the display, place the single argument in %rdi
(Pascaline's first parameter register under the amd64 SysV convention) and call
the code. %rbp is saved and restored so the C caller is unaffected.

********************************************************************************/

__asm__ (
"    .text\n"
"    .globl pacall\n"
"pacall:\n"                 /* void pacall(code=%rdi, display=%rsi, arg=%rdx) */
"    push %rbp\n"           /* preserve C frame pointer */
"    mov  %rdi, %rax\n"     /* %rax = code address */
"    mov  %rsi, %rbp\n"     /* %rbp = display (static link) */
"    mov  %rdx, %rdi\n"     /* %rdi = argument (Pascaline param 1) */
"    call *%rax\n"          /* enter the Pascaline procedure */
"    pop  %rbp\n"           /* restore C frame pointer */
"    ret\n"
);

extern void pacall(void* code, void* display, void* arg);

/* Demonstration: call the passed Pascaline procedure immediately. The procedure
   parameter arrives as (code, display) in consecutive argument registers. */
void wrapper_democall(
    /** procedure code */    void* code,
    /** procedure display */ void* display
)

{

    pacall(code, display, (void*)0);

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

#include <sys/mman.h>

/* dispatch an event callback: convert the C event, enter the Pascaline handler,
   then copy the handled flag back */
void evtdispatch(void* code, void* display, ami_evtrec* cer)
{

    char per[80]; /* Pascaline event record buffer */

    cevt2pascaline(cer, per);
    pacall(code, display, per);
    cer->handled = PEVTC(per, PA_EVT_HANDLED);

}

/* dispatch a thread body callback (no arguments) */
void thrdispatch(void* code, void* display)
{

    pacall(code, display, (void*)0);

}

#define THUNKSZ 48  /* bytes reserved per thunk */
#define NTHUNK  64  /* number of thunks in the pool */

static unsigned char* thunkpool = 0; /* executable thunk pool */
static int            thunknext = 0; /* next free thunk */

static unsigned char* thunkalloc(void)
{

    if (!thunkpool)
        thunkpool = mmap(0, THUNKSZ*NTHUNK, PROT_READ|PROT_WRITE|PROT_EXEC,
                         MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    return thunkpool+(thunknext++ % NTHUNK)*THUNKSZ;

}

/* emit a movabs of a 64 bit immediate into a register (opcode selects reg) */
static unsigned char* emitimm(unsigned char* p, unsigned char op, void* val)
{

    *p++ = 0x48; *p++ = op;
    memcpy(p, &val, 8);
    return p+8;

}

/* build an event thunk: void thunk(ami_evtrec* cer) -> evtdispatch(code,display,cer) */
static void* mkevtthunk(void* code, void* display)
{

    unsigned char* t = thunkalloc();
    unsigned char* p = t;

    *p++ = 0x48; *p++ = 0x89; *p++ = 0xfa;   /* mov  %rdi,%rdx  (cer -> arg3) */
    p = emitimm(p, 0xbe, display);           /* movabs display,%rsi (arg2)   */
    p = emitimm(p, 0xbf, code);              /* movabs code,%rdi    (arg1)   */
    p = emitimm(p, 0xb8, (void*)evtdispatch);/* movabs evtdispatch,%rax      */
    *p++ = 0xff; *p++ = 0xe0;                /* jmp  *%rax                   */

    return t;

}

/* build a thread thunk: void thunk(void) -> thrdispatch(code,display) */
static void* mkthrthunk(void* code, void* display)
{

    unsigned char* t = thunkalloc();
    unsigned char* p = t;

    p = emitimm(p, 0xbe, display);           /* movabs display,%rsi (arg2) */
    p = emitimm(p, 0xbf, code);              /* movabs code,%rdi    (arg1) */
    p = emitimm(p, 0xb8, (void*)thrdispatch);/* movabs thrdispatch,%rax    */
    *p++ = 0xff; *p++ = 0xe0;                /* jmp  *%rax                 */

    return t;

}

/********************************************************************************

Event and thread override wrappers

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

    ami_eventover(e, (ami_pevthan)mkevtthunk(eh_code, eh_display), &old);
    *(void**)oeh = (void*)old;

}

void wrapper_eventsover(
    /** handler code */    void* eh_code,
    /** handler display */ void* eh_display,
    /** old handler out */ void* oeh
)

{

    ami_pevthan old;

    ami_eventsover((ami_pevthan)mkevtthunk(eh_code, eh_display), &old);
    *(void**)oeh = (void*)old;

}

int wrapper_newthread(
    /** thread body code */    void* code,
    /** thread body display */ void* display
)

{

    return ami_newthread((void(*)(void))mkthrthunk(code, display));

}

/* Demonstration: build an event handler thunk (exactly as eventover does) and
   fire a synthetic event through it, just as Petit-Ami would when the installed
   handler is invoked. Exercises the whole path: thunk -> evtdispatch -> event
   record conversion (including the reverse-order variant fields) -> pacall ->
   the Pascaline handler. Used because a live event needs a real terminal. */
void wrapper_demoevent(
    /** handler code */    void* code,
    /** handler display */ void* display
)

{

    void (*thunk)(ami_evtrec*); /* the installable C handler */
    ami_evtrec ce;              /* synthetic C event record */

    thunk = (void(*)(ami_evtrec*))mkevtthunk(code, display);
    memset(&ce, 0, sizeof(ce));
    ce.etype = ami_etmoumov;    /* a mouse move, with multi-field data */
    ce.mmoun = 3;
    ce.moupx = 11;
    ce.moupy = 22;
    thunk(&ce);                 /* fire it, as the event dispatcher would */

}
