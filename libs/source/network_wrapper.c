/* Network wrappers (originally generated, now hand maintained).
 *
 * Compiled against plain glibc stdio (no STDIO_BYPASS): the network
 * library lives in the glibc world. Only the string conversion
 * helpers are shared with the bypass world; they touch no FILE.
 *
 * Types: the Ami API integer is ami_long (localdefs.h), the machine word:
 * long on SysV, long long on 64 bit windows (where plain long is only 32
 * bits). That is the Pascaline integer, so integer parameters and results
 * are ami_long and pass through unconverted. Addresses are Pascaline
 * lcardinal, 64 bits, and so long long / long long* here; the ami IPv4
 * address type is ami_ulong, and v4 addresses convert through a local of
 * that type.
 */

#include <network.h>

extern char* cstrz(char* s, int l); /* support.o: trim pad + terminate */

void wrapper_addrnet(char* name, int namel, long long* addr)
{
    ami_ulong a; /* ami's v4 address type */

    ami_addrnet(cstrz(name, namel), &a);
    *addr = a; /* widen to the 64 bit Pascaline integer */
}

void wrapper_addrnetv6(char* name, int namel, long long* addrh, long long* addrl)
{
    ami_addrnetv6(cstrz(name, namel), (unsigned long long*)addrh, (unsigned long long*)addrl);
}

ami_long wrapper_maxmsg(long long addr, ami_long secure)
{
    return ami_maxmsg((ami_ulong)addr, secure);
}

ami_long wrapper_maxmsgv6(long long addrh, long long addrl, ami_long secure)
{
    return ami_maxmsgv6((unsigned long long)addrh, (unsigned long long)addrl, secure);
}

ami_long wrapper_relymsg(long long addr)
{
    return ami_relymsg((ami_ulong)addr);
}

ami_long wrapper_relymsgv6(long long addrh, long long addrl)
{
    return ami_relymsgv6((unsigned long long)addrh, (unsigned long long)addrl);
}

ami_long wrapper_openmsg(long long addr, ami_long port, ami_long secure)
{
    return ami_openmsg((ami_ulong)addr, port, secure);
}

ami_long wrapper_openmsgv6(long long addrh, long long addrl, ami_long port, ami_long secure)
{
    return ami_openmsgv6((unsigned long long)addrh, (unsigned long long)addrl, port, secure);
}

void wrapper_wrmsg(ami_long fn, char* msg, int msgl)
{
    ami_wrmsg(fn, (void*)msg, msgl);
}

ami_long wrapper_rdmsg(ami_long fn, char* msg, int msgl)
{
    return ami_rdmsg(fn, (void*)msg, msgl);
}

ami_long wrapper_rdymsg(ami_long fn, ami_long usec)
{
    return ami_rdymsg(fn, usec);
}

void wrapper_tmomsg(ami_long fn, ami_long usec)
{
    ami_tmomsg(fn, usec);
}

void wrapper_bufmsg(ami_long fn, ami_long len)
{
    ami_bufmsg(fn, len);
}

void wrapper_shutmsg(ami_long fn)
{
    ami_shutmsg(fn);
}

void wrapper_clsmsg(ami_long fn)
{
    ami_clsmsg(fn);
}

ami_long wrapper_waitmsg(ami_long port, ami_long secure)
{
    return ami_waitmsg(port, secure);
}

ami_long wrapper_certmsg(ami_long fn, ami_long which, char* cert, int certl)
{
    ami_long r;

    r = ami_certmsg(fn, which, cert, certl);
    /* the out string is space padded back for Pascaline (this ran after
       the return before, and so never executed) */
    { int _p = 0; while (_p < certl && cert[_p]) _p++;
      while (_p < certl) cert[_p++] = ' '; }
    return r;
}
