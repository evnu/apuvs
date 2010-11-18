// =====================================================================================
//       Filename:  messuretime.h
//    Description:  messure time
//        Created:  12.11.2010 17:52:07
// =====================================================================================

// time difference in milliseconds
#include <sys/time.h>
double mdiff (struct timeval *start, struct timeval *end);
