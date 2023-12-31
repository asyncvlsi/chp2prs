#include "reqs.h"
#include "synthesis_struct.h"


Act *a_mangle;

static const int style_global = 1;

void mangle_init ()
{ 
  char u[5];
  a_mangle = new Act;
  snprintf(u,5,"[],");
  a_mangle->mangle(u);
}

void get_true_name (char *buf, ActId *id, Scope *s)
{
    char str[1024], t[1024];
    if (!(id->rootVx(s)->t->arrayInfo()))
        id->sPrint(buf,1024,NULL,style_global);
    else{
        id->sPrint(str,1024,NULL,style_global);
        a_mangle->mangle_string(str,buf,1024);
    }
}

void generate_array_suffix(char *buf, Array *a)
{
    char s[1024];
    a->sPrint(s,1024,style_global);
    a_mangle->mangle_string(s,buf,1024);
    // return buf;
}