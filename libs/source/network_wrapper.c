/* Generated network wrappers. Do not edit by hand.
 *
 * Compiled against plain glibc stdio (no STDIO_BYPASS): the network
 * library lives in the glibc world. Only the string conversion
 * helpers are shared with the bypass world; they touch no FILE.
 */

#include <network.h>

extern char* cstrz(char* s, int l); /* support.o: trim pad + terminate */

void wrapper_addrnet(char* name, int namel, long* addr)
{
    ami_addrnet(cstrz(name, namel), (unsigned long*)addr);
}

void wrapper_addrnetv6(char* name, int namel, long* addrh, long* addrl)
{
    ami_addrnetv6(cstrz(name, namel), (unsigned long long*)addrh, (unsigned long long*)addrl);
}

int wrapper_maxmsg(long addr)
{
    return ami_maxmsg(addr);
}

int wrapper_maxmsgv6(long addrh, long addrl)
{
    return ami_maxmsgv6(addrh, addrl);
}

int wrapper_relymsg(long addr)
{
    return ami_relymsg(addr);
}

int wrapper_relymsgv6(long addrh, long addrl)
{
    return ami_relymsgv6(addrh, addrl);
}

int wrapper_openmsg(long addr, int port, int secure)
{
    return ami_openmsg(addr, port, secure);
}

int wrapper_openmsgv6(long addrh, long addrl, int port, int secure)
{
    return ami_openmsgv6(addrh, addrl, port, secure);
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
    return ami_certmsg(fn, which, cert, certl);
    { int _p = 0; while (_p < certl && cert[_p]) _p++;
      while (_p < certl) cert[_p++] = ' '; }
}

