/*******************************************************************************
*                                                                              *
*                  Common Pascaline/C Support for Ami Bindings                 *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Shared conversions and call mechanisms used by the Pascaline bindings         *
* (services, terminal, graphics, ...): string conversions, and the machinery    *
* to call a Pascaline procedure and build event callback thunks from C. None of *
* this depends on a particular binding's module header, so it is factored here  *
* and linked into each binding's archive. See support.h.              *
*                                                                              *
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <support.h>

/********************************************************************************

String conversions

********************************************************************************/

/* C zero-terminated string (within a buffer of length l) -> heap pstring */
pstring cstr2pstr(char* cs, int l)
{
    pstring ps;
    char*   s;

    l = strnlen(cs, l);
    ps = malloc(sizeof(pstring_header)+l);
    ps->len = l;
    s = (char*)&ps->data;
    strncpy(s, cs, l);

    return (ps);
}

/* pstring -> heap zero-terminated C string */
char* pstr2cstr(pstring ps)
{
    char* cs;

    cs = malloc(ps->len+1);
    memcpy(cs, (char*)&ps->data, ps->len);
    cs[ps->len] = 0;

    return (cs);
}

/* pstring -> (char*, length) referencing the data in place */
void pstr2cstrl(pstring ps, char** s, int* l)
{
    *l = ps->len;
    *s = (char*)&ps->data;
}

/* pstring -> (char*, length) in place (pointer rewritten to the data) */
void cpstrp2cstrl(pstring* ps, int* l)
{
    *l = (*ps)->len;
    *ps = (pstring)&((*ps)->data);
}

/* fill the buffer past the C string with spaces, including the terminator */
void cstr2pad(char* cs, int l)
{
    while (l && *cs) { l--; cs++; }
    while (l) { l--; *cs++ = ' '; }
}

/* remove right space padding from a (string, length) pair by trimming length */
void rempad(char* cs, int* l)
{
    cs += *l-1;
    while (*l && *cs == ' ') { (*l)--; cs--; }
}

/*
 * Counted Pascaline string -> zero-terminated C string.
 *
 * Pascaline passes a string as a (pointer, length) pair that is not terminated;
 * the Ami ami_* APIs take zero-terminated C strings. A rotating pool of
 * per-thread buffers lets several string arguments of one call coexist.
 */
#define CSTRZBUF 8    /* number of rotating conversion buffers */
#define CSTRZLEN 1024 /* maximum converted string length */

char* cstrz(char* s, int l)
{
    static __thread char buff[CSTRZBUF][CSTRZLEN];
    static __thread int  nxt = 0;
    char* b;

    b = buff[nxt];
    nxt = (nxt+1)%CSTRZBUF;
    if (l > CSTRZLEN-1) l = CSTRZLEN-1;
    memcpy(b, s, l);
    b[l] = 0;

    return (b);
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

/********************************************************************************

Callback thunks

Ami stores a callback as a single C function pointer and calls it later
(deferred). A Pascaline procedure is two words (code, display), so it cannot be
handed over directly. For each installed callback we build a small machine code
thunk that captures (code, display) and the dispatcher address, and presents the
plain C function pointer Ami expects. When Ami calls the thunk, the
thunk tail-calls the dispatcher, which converts arguments and uses pacall() to
enter the Pascaline procedure with the captured display.

The thunks live in a fixed executable pool, so they persist for the life of the
program. The dispatcher is supplied by the caller, because converting the event
record is specific to each binding's module.

********************************************************************************/

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

/* build a thunk: void thunk(void* arg) -> dispatch(code, display, arg) */
void* mkevtthunk(void* code, void* display, void* dispatch)
{
    unsigned char* t = thunkalloc();
    unsigned char* p = t;

    *p++ = 0x48; *p++ = 0x89; *p++ = 0xfa;   /* mov  %rdi,%rdx  (arg -> arg3)  */
    p = emitimm(p, 0xbe, display);           /* movabs display,%rsi (arg2)    */
    p = emitimm(p, 0xbf, code);              /* movabs code,%rdi    (arg1)    */
    p = emitimm(p, 0xb8, dispatch);          /* movabs dispatch,%rax          */
    *p++ = 0xff; *p++ = 0xe0;                /* jmp  *%rax                    */

    return t;
}
