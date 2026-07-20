/* Network wrappers (originally generated, now hand maintained).
 *
 * Compiled against plain glibc stdio (no STDIO_BYPASS): the network
 * library lives in the glibc world. Only the string conversion
 * helpers are shared with the bypass world; they touch no FILE.
 *
 * Types: a Pascaline integer is 64 bits on every platform, so the
 * Pascaline-facing parameters use long long / long long* explicitly
 * (plain long is only 32 bits on windows). The ami IPv4 address type
 * is unsigned long -- 64 bits on SysV, 32 bits on windows -- so v4
 * addresses convert through a local of ami's own type.
 */

#include <network.h>

extern char* cstrz(char* s, int l); /* support.o: trim pad + terminate */

void wrapper_addrnet(char* name, int namel, long long* addr)
{
    unsigned long a; /* ami's v4 address type (64 bits SysV, 32 windows) */

    ami_addrnet(cstrz(name, namel), &a);
    *addr = a; /* widen to the 64 bit Pascaline integer */
}

void wrapper_addrnetv6(char* name, int namel, long long* addrh, long long* addrl)
{
    ami_addrnetv6(cstrz(name, namel), (unsigned long long*)addrh, (unsigned long long*)addrl);
}

int wrapper_maxmsg(long long addr)
{
    return ami_maxmsg((unsigned long)addr);
}

int wrapper_maxmsgv6(long long addrh, long long addrl)
{
    return ami_maxmsgv6((unsigned long long)addrh, (unsigned long long)addrl);
}

int wrapper_relymsg(long long addr)
{
    return ami_relymsg((unsigned long)addr);
}

int wrapper_relymsgv6(long long addrh, long long addrl)
{
    return ami_relymsgv6((unsigned long long)addrh, (unsigned long long)addrl);
}

int wrapper_openmsg(long long addr, int port, int secure)
{
    return ami_openmsg((unsigned long)addr, port, secure);
}

int wrapper_openmsgv6(long long addrh, long long addrl, int port, int secure)
{
    return ami_openmsgv6((unsigned long long)addrh, (unsigned long long)addrl, port, secure);
}

void wrapper_wrmsg(int fn, char* msg, int msgl)
{
    ami_wrmsg(fn, (void*)msg, msgl);
}

int wrapper_rdmsg(int fn, char* msg, int msgl)
{
    return ami_rdmsg(fn, (void*)msg, msgl);
}

void wrapper_clsmsg(int f)
{
    ami_clsmsg(f);
}

int wrapper_waitmsg(int port, int secure)
{
    return ami_waitmsg(port, secure);
}

int wrapper_certmsg(int fn, int which, char* cert, int certl)
{
    int r;

    r = ami_certmsg(fn, which, cert, certl);
    /* the out string is space padded back for Pascaline (this ran after
       the return before, and so never executed) */
    { int _p = 0; while (_p < certl && cert[_p]) _p++;
      while (_p < certl) cert[_p++] = ' '; }
    return r;
}
