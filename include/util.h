#ifndef _UTIL_H
#define _UTIL_H

#ifdef OMEGA_STUB_DEBUG
#define OMEGA_STUB_DEBUG_MSG(msg...) { fprintf(stdout, msg); fflush(stdout); }
#else
#define OMEGA_STUB_DEBUG_MSG(msg...)
#endif

#endif
