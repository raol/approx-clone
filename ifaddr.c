/* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License */

#include <string.h>
#include <unistd.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>

static int
ifaddr(char *name, /* OUT */ struct in_addr *addr)
{
    struct ifreq ifr;
    int s, r;

    s = socket(AF_INET, SOCK_DGRAM, IPPROTO_IP);
    if (s == -1)
        return 0;
    strncpy(ifr.ifr_name, name, sizeof(ifr.ifr_name));
    ifr.ifr_addr.sa_family = AF_INET;
    r = ioctl(s, SIOCGIFADDR, &ifr);
    close(s);
    if (r == -1)
        return 0;
    *addr = ((struct sockaddr_in *) &ifr.ifr_addr)->sin_addr;
    return 1;
}

#include <caml/fail.h>
#include <caml/memory.h>

/*
 * defined in otherlibs/unix/socketaddr.c
 */
extern value alloc_inet_addr(struct in_addr *);

value
interface_address(value name)
{
    CAMLparam1(name);
    struct in_addr sin;

    if (ifaddr(String_val(name), &sin))
        CAMLreturn(alloc_inet_addr(&sin));
    else
        caml_raise_not_found();
}
