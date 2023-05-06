/******************************************************************************
*                                                                             *
*                         TESTS FOR EXTLIB                                    *
*                                                                             *
*                   Copyright (C) 2001 S. A. Moore                            *
*                                                                             *
******************************************************************************/

#include <stdio.h>

extern void pa_getenv(char* ls, char*ds, int dsl);

void main()

{

   char s[40];

   pa_getenv("bark", s, 40);
   printf("This is exttst1: \'%s\'\n", s);

}