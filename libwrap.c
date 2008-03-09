/* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License */

#include <tcpd.h>
#include <caml/memory.h>

value
wrap_hosts_ctl(value daemon, value host, value address, value user)
{
    CAMLparam4(daemon, host, address, user);
    CAMLreturn(Val_int(hosts_ctl(String_val(daemon), String_val(host),
                                 String_val(address), String_val(user)) != 0));
}
