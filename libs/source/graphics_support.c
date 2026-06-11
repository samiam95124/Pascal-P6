/*******************************************************************************
*                                                                              *
*                     Graphics Library Support for Pascaline                   *
*                                                                              *
*                              Created 2024                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Conversion helpers and hand-written wrappers for the Pascaline graphics       *
* binding: event record translation, the procedure-parameter callback layer,    *
* and the wrappers whose arguments need marshalling (menus, string lists,        *
* event records). The straight parameter wrappers live in graphics_wrapper.c.    *
*                                                                              *
* Graphics is a superset of terminal; the event record is the terminal record    *
* extended with the graphical-coordinate and widget events.                      *
*                                                                              *
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <graphics.h>
#include <support.h>

/********************************************************************************

Convert C event record to Pascaline event record

The Pascaline 'evtrec' variant record (graphics.pas) lays the fixed fields out
forward (winid@0, handled@8, etype@9) and the variant fields from offset 12 in
REVERSE declaration order, each 8 bytes (echar is a single byte). The named
event codes are in the same order in the C and Pascaline enumerations, so the
tag byte copies directly.

********************************************************************************/

#define PD 12 /* base of variant data */

void cevt2pascaline(ami_evtrec* ce, char* pe)
{
    PEVTL(pe, PA_EVT_WINID)   = ce->winid;
    PEVTC(pe, PA_EVT_HANDLED) = ce->handled;
    PEVTC(pe, PA_EVT_ETYPE)   = ce->etype;
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
        case ami_etresize: PEVTL(pe, PD)    = ce->rszyg;  /* rszx,rszy,rszxg,rszyg */
                           PEVTL(pe, PD+8)  = ce->rszxg;
                           PEVTL(pe, PD+16) = ce->rszy;
                           PEVTL(pe, PD+24) = ce->rszx;   break;
        case ami_etmoumovg:PEVTL(pe, PD)    = ce->moupyg; /* mmoung,moupxg,moupyg */
                           PEVTL(pe, PD+8)  = ce->moupxg;
                           PEVTL(pe, PD+16) = ce->mmoung; break;
        case ami_etredraw: PEVTL(pe, PD)    = ce->rey;    /* rsx,rsy,rex,rey */
                           PEVTL(pe, PD+8)  = ce->rex;
                           PEVTL(pe, PD+16) = ce->rsy;
                           PEVTL(pe, PD+24) = ce->rsx;    break;
        case ami_etmenus:  PEVTL(pe, PD)    = ce->menuid; break;
        case ami_etbutton: PEVTL(pe, PD)    = ce->butid;  break;
        case ami_etchkbox: PEVTL(pe, PD)    = ce->ckbxid; break;
        case ami_etradbut: PEVTL(pe, PD)    = ce->radbid; break;
        case ami_etsclull: PEVTL(pe, PD)    = ce->sclulid; break;
        case ami_etscldrl: PEVTL(pe, PD)    = ce->scldrid; break;
        case ami_etsclulp: PEVTL(pe, PD)    = ce->sclupid; break;
        case ami_etscldrp: PEVTL(pe, PD)    = ce->scldpid; break;
        case ami_etsclpos: PEVTL(pe, PD)    = ce->sclpos; /* sclpid,sclpos */
                           PEVTL(pe, PD+8)  = ce->sclpid; break;
        case ami_etedtbox: PEVTL(pe, PD)    = ce->edtbid; break;
        case ami_etnumbox: PEVTL(pe, PD)    = ce->numbsl; /* numbid,numbsl */
                           PEVTL(pe, PD+8)  = ce->numbid; break;
        case ami_etlstbox: PEVTL(pe, PD)    = ce->lstbsl; /* lstbid,lstbsl */
                           PEVTL(pe, PD+8)  = ce->lstbid; break;
        case ami_etdrpbox: PEVTL(pe, PD)    = ce->drpbsl; /* drpbid,drpbsl */
                           PEVTL(pe, PD+8)  = ce->drpbid; break;
        case ami_etdrebox: PEVTL(pe, PD)    = ce->drebid; break;
        case ami_etsldpos: PEVTL(pe, PD)    = ce->sldpos; /* sldpid,sldpos */
                           PEVTL(pe, PD+8)  = ce->sldpid; break;
        case ami_ettabbar: PEVTL(pe, PD)    = ce->tabsel; /* tabid,tabsel */
                           PEVTL(pe, PD+8)  = ce->tabid;  break;
        default: break; /* events without parameter data */

    }
}

/* Convert a Pascaline event record back to C form (for sendevent). Only the
   commonly sent events carry data; others send just the tag. */
static void pascaline2cevt(char* pe, ami_evtrec* ce)
{
    memset(ce, 0, sizeof(*ce));
    ce->winid   = PEVTL(pe, PA_EVT_WINID);
    ce->handled = PEVTC(pe, PA_EVT_HANDLED);
    ce->etype   = (ami_evtcod)(unsigned char)PEVTC(pe, PA_EVT_ETYPE);
    switch (ce->etype) {
        case ami_etchar:   ce->echar  = PEVTC(pe, PD); break;
        case ami_ettim:    ce->timnum = PEVTL(pe, PD); break;
        case ami_etmoumov: ce->moupy=PEVTL(pe,PD); ce->moupx=PEVTL(pe,PD+8);
                           ce->mmoun=PEVTL(pe,PD+16); break;
        case ami_etfun:    ce->fkey = PEVTL(pe, PD); break;
        case ami_etmenus:  ce->menuid = PEVTL(pe, PD); break;
        default: break;
    }
}

/********************************************************************************
Event wrappers
********************************************************************************/

void wrapper_eventf(pfile pfp, char* per)
{
    FILE* f = psystem_libcrdfil(pfp);
    ami_evtrec ce;
    ami_event(f, &ce);
    cevt2pascaline(&ce, per);
}

void wrapper_event(char* per)
{
    ami_evtrec ce;
    ami_event(stdin, &ce);
    cevt2pascaline(&ce, per);
}

void wrapper_sendeventf(pfile pfp, char* per)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_evtrec ce;
    pascaline2cevt(per, &ce);
    ami_sendevent(f, &ce);
}

void wrapper_sendevent(char* per)
{
    ami_evtrec ce;
    pascaline2cevt(per, &ce);
    ami_sendevent(stdout, &ce);
}

/******************************************************************************

Dispatch an event callback

Convert the C event record to Pascaline form, enter the Pascaline handler
through the shared pacall(), then copy the handled flag back. The shared
mkevtthunk() is given this routine as the dispatcher for a callback thunk.

******************************************************************************/

void evtdispatch(void* code, void* display, ami_evtrec* cer)
{
    char per[80];
    cevt2pascaline(cer, per);
    pacall(code, display, per);
    cer->handled = PEVTC(per, PA_EVT_HANDLED);
}

void wrapper_eventover(int e, void* eh_code, void* eh_display, void* oeh)
{
    ami_pevthan old;
    ami_eventover(e, (ami_pevthan)mkevtthunk(eh_code, eh_display, (void*)evtdispatch), &old);
    *(void**)oeh = (void*)old;
}

void wrapper_eventsover(void* eh_code, void* eh_display, void* oeh)
{
    ami_pevthan old;
    ami_eventsover((ami_pevthan)mkevtthunk(eh_code, eh_display, (void*)evtdispatch), &old);
    *(void**)oeh = (void*)old;
}

/********************************************************************************

Menu and string-list marshalling

The Pascaline menurec/strrec records use pstring text and 8-byte boolean flags;
the C ami_menurec/ami_strrec use char* and int. Convert the Pascaline linked
list into a C linked list allocated for the call.

********************************************************************************/

/* Pascaline menurec layout (graphics.pas), from the mangled record type
   (next$0$p2$branch$8$p2$onoff$16$b$oneof$17$b$bar$18$b$id$20$i$face$28$pvc):
   pointers and the integer are 8 bytes, the booleans single bytes. */
#define PM_NEXT   0  /* menuptr */
#define PM_BRANCH 8  /* menuptr */
#define PM_ONOFF  16 /* boolean (byte) */
#define PM_ONEOF  17 /* boolean (byte) */
#define PM_BAR    18 /* boolean (byte) */
#define PM_ID     20 /* integer (8 bytes) */
#define PM_FACE   28 /* pstring */
#define PM_SIZE   36 /* size of record */
#define PMP(p,off) (*(void**)((char*)(p)+(off)))
#define PMI(p,off) (*(long*) ((char*)(p)+(off)))
#define PMC(p,off) (*(char*) ((char*)(p)+(off)))


static ami_menuptr cvtmenu(void* pm)
{
    ami_menuptr m;
    if (!pm) return 0;
    m = malloc(sizeof(ami_menurec));
    m->next   = cvtmenu(PMP(pm, PM_NEXT));
    m->branch = cvtmenu(PMP(pm, PM_BRANCH));
    m->onoff  = PMC(pm, PM_ONOFF) != 0;
    m->oneof  = PMC(pm, PM_ONEOF) != 0;
    m->bar    = PMC(pm, PM_BAR)   != 0;
    m->id     = PMI(pm, PM_ID);
    m->face   = pstr2cstr((pstring)PMP(pm, PM_FACE));
    return m;
}

/* Convert a C menu list to Pascaline menurec form (for stdmenu's result).
   The records are heap blocks laid out as Pascaline reads them. */
static void* cvtmenu2pa(ami_menuptr m)
{
    char* pm;
    if (!m) return 0;
    pm = calloc(1, PM_SIZE);
    PMP(pm, PM_NEXT)   = cvtmenu2pa(m->next);
    PMP(pm, PM_BRANCH) = cvtmenu2pa(m->branch);
    PMC(pm, PM_ONOFF)  = m->onoff != 0;
    PMC(pm, PM_ONEOF)  = m->oneof != 0;
    PMC(pm, PM_BAR)    = m->bar   != 0;
    PMI(pm, PM_ID)     = m->id;
    PMP(pm, PM_FACE)   = cstr2pstr(m->face, strlen(m->face));
    return pm;
}

void wrapper_menuf(pfile pfp, void* m)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_menu(f, cvtmenu(m)); /* note: allocates a C copy for the call */
}
void wrapper_menu(void* m) { ami_menu(stdout, cvtmenu(m)); }

/* Pascaline strrec layout: next@0, str(pstring)@8 */
static ami_strptr cvtstrlst(void* ps)
{
    ami_strptr s;
    if (!ps) return 0;
    s = malloc(sizeof(ami_strrec));
    s->next = cvtstrlst(PMP(ps, 0));
    s->str  = pstr2cstr((pstring)PMP(ps, 8));
    return s;
}

void wrapper_stdmenu(int sms, void* sm, void* pm)
{
    /* stdmenu(sms; var sm: menuptr; pm: menuptr) builds a standard menu list
       and returns it through the var parameter in Pascaline form. */
    ami_menuptr csm;
    ami_stdmenu(sms, &csm, cvtmenu(pm));
    *(void**)sm = cvtmenu2pa(csm);
}

/* string-list widgets: listbox/dropbox/dropeditbox and their *g/siz variants */
#define LSTBOX(NM) \
void wrapper_##NM##f(pfile pfp, int x1,int y1,int x2,int y2, void* sp, int id) \
{ FILE* f = psystem_libcwrfil(pfp); ami_##NM(f,x1,y1,x2,y2,cvtstrlst(sp),id); } \
void wrapper_##NM(int x1,int y1,int x2,int y2, void* sp, int id) \
{ ami_##NM(stdout,x1,y1,x2,y2,cvtstrlst(sp),id); }
LSTBOX(listbox)
LSTBOX(listboxg)
LSTBOX(dropbox)
LSTBOX(dropboxg)
LSTBOX(dropeditbox)
LSTBOX(dropeditboxg)

/* size queries: strptr + 2 out ints (listbox) */
#define LSTSIZ2(NM) \
void wrapper_##NM##f(pfile pfp, void* sp, long* w, long* h) \
{ FILE* f=psystem_libcwrfil(pfp); int tw,th; ami_##NM(f,cvtstrlst(sp),&tw,&th); \
  *w=tw; *h=th; } \
void wrapper_##NM(void* sp, long* w, long* h) \
{ int tw,th; ami_##NM(stdout,cvtstrlst(sp),&tw,&th); *w=tw; *h=th; }
LSTSIZ2(listboxsiz)
LSTSIZ2(listboxsizg)

/* size queries: strptr + 4 out ints (drop box closed/open width/height) */
#define LSTSIZ4(NM) \
void wrapper_##NM##f(pfile pfp, void* sp, long* cw, long* ch, long* ow, long* oh) \
{ FILE* f=psystem_libcwrfil(pfp); int a,b,c,d; ami_##NM(f,cvtstrlst(sp),&a,&b,&c,&d); \
  *cw=a; *ch=b; *ow=c; *oh=d; } \
void wrapper_##NM(void* sp, long* cw, long* ch, long* ow, long* oh) \
{ int a,b,c,d; ami_##NM(stdout,cvtstrlst(sp),&a,&b,&c,&d); *cw=a;*ch=b;*ow=c;*oh=d; }
LSTSIZ4(dropboxsiz)
LSTSIZ4(dropboxsizg)
LSTSIZ4(dropeditboxsiz)
LSTSIZ4(dropeditboxsizg)

/* tab bar: coords + strptr + orientation + id */
#define TABBAR(NM) \
void wrapper_##NM##f(pfile pfp, int x1,int y1,int x2,int y2, void* sp, int tor, int id) \
{ FILE* f=psystem_libcwrfil(pfp); ami_##NM(f,x1,y1,x2,y2,cvtstrlst(sp),tor,id); } \
void wrapper_##NM(int x1,int y1,int x2,int y2, void* sp, int tor, int id) \
{ ami_##NM(stdout,x1,y1,x2,y2,cvtstrlst(sp),tor,id); }
TABBAR(tabbar)
TABBAR(tabbarg)

/********************************************************************************

openwin and the query dialogs

********************************************************************************/

/*
 * Open a window. ami_openwin takes the C file handles by reference: the input
 * side is normally the shared stdin (events for every window arrive on it),
 * and the output side comes back as a new C file for the window. The new
 * handles are bound to the caller's Pascaline file variables with
 * psystem_libcatcfil so subsequent Pascaline I/O on them routes to the window.
 *
 * The parent is a window output file, or an unopened file variable (logical
 * file 0) for a top-level window -- the Pascaline expression of C's NULL
 * parent.
 */
void wrapper_openwin(pfile infile, pfile outfile, pfile parent, int wid)
{
    FILE* fin;
    FILE* fout;
    FILE* par;

    /* input side: the caller's file if open, else the shared stdin */
    fin = *infile ? psystem_libcrdfil(infile) : stdin;
    fout = NULL;
    par = *parent ? psystem_libcwrfil(parent) : NULL;
    ami_openwin(&fin, &fout, par, wid);
    /* bind the window files to the Pascaline file variables */
    if (*infile == 0) psystem_libcatcfil(infile, fin, 0);
    psystem_libcatcfil(outfile, fout, 1);
}

/* query dialogs: out strings and an option set (int). Require the widget
   package to actually run; the option set is copied as an int bitmask. */
void wrapper_queryfind(string s, int sl, long* opt)
{
    int topt = (int)*opt;
    ami_queryfind(s, sl, &topt);
    *opt = topt;
}

void wrapper_queryfindrep(string s, int sl, string r, int rl, long* opt)
{
    int topt = (int)*opt;
    ami_queryfindrep(s, sl, r, rl, &topt);
    *opt = topt;
}

void wrapper_queryfontf(pfile pfp, long* fc, long* s, long* fr, long* fg,
                        long* fb, long* br, long* bg, long* bb, long* effect)
{
    FILE* f = psystem_libcwrfil(pfp);
    int afc,as,afr,afg,afb,abr,abg,abb,aeff;
    afc=*fc; as=*s; afr=*fr; afg=*fg; afb=*fb; abr=*br; abg=*bg; abb=*bb; aeff=*effect;
    ami_queryfont(f, &afc,&as,&afr,&afg,&afb,&abr,&abg,&abb,&aeff);
    *fc=afc; *s=as; *fr=afr; *fg=afg; *fb=afb; *br=abr; *bg=abg; *bb=abb; *effect=aeff;
}

void wrapper_queryfont(long* fc, long* s, long* fr, long* fg, long* fb,
                       long* br, long* bg, long* bb, long* effect)
{
    int afc,as,afr,afg,afb,abr,abg,abb,aeff;
    afc=*fc; as=*s; afr=*fr; afg=*fg; afb=*fb; abr=*br; abg=*bg; abb=*bb; aeff=*effect;
    ami_queryfont(stdout, &afc,&as,&afr,&afg,&afb,&abr,&abg,&abb,&aeff);
    *fc=afc; *s=as; *fr=afr; *fg=afg; *fb=afb; *br=abr; *bg=abg; *bb=abb; *effect=aeff;
}
