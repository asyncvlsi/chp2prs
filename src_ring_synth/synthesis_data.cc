#include "reqs.h"
#include "synthesis_struct.h"

// Refer to synthesis_struct.h to see what a var_info object holds.
// void generate_array_suffix(char *, Array *, Act *, int);
// const char *get_true_name (ActId *, Scope *);

/*
    Print the contents of the var_info object. Some fields are 
    omitted, can be added. 
*/
void print_var_info (FILE *fp, var_info *v)
{
  if (!v) return;
  fprintf(fp, "\nName: %s", v->name);
  fprintf(fp, "\nBitwidth: %d", v->width);
  fprintf(fp, "\nNo. of reads: %d", v->nread);
  fprintf(fp, "\nNo. of writes: %d", v->nwrite);
  fprintf(fp, "\n\n");
  // fprintf(fp, "\nArray size (0 if not array): %d\n\n", v->array_size);
}

/*
    Checks if a given variable appears in an expression.
*/
bool var_appears_in_expr (Expr *e, ValueIdx *vx, Scope *s, var_info *v)
{
  act_connection *uid;
  ActId *i;
  ValueIdx *vix;
  Array *a;
  Arraystep *as;
  bool a1, a2, a3;
  char str[1024], t[1024];
  
  if (!e) return false;
  switch (e->type) {
    /* binary */
  case E_AND:
  case E_OR:
  case E_PLUS:
  case E_MINUS:
  case E_MULT:
  case E_DIV:
  case E_MOD:
  case E_LSL:
  case E_LSR:
  case E_ASR:
  case E_XOR:
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
    a1 = var_appears_in_expr (e->u.e.l, vx, s, v);
    a2 = var_appears_in_expr (e->u.e.r, vx, s, v);
    return a1 | a2;
    break;
    
  case E_NOT:
  case E_UMINUS:
  case E_COMPLEMENT:
    a1 = var_appears_in_expr (e->u.e.l, vx, s, v);
    return a1;
    break;

  case E_QUERY:
    a1 = var_appears_in_expr (e->u.e.l, vx, s, v);
    a2 = var_appears_in_expr (e->u.e.r->u.e.l, vx, s, v);
    a3 = var_appears_in_expr (e->u.e.r->u.e.r, vx, s, v);
    return a1 | a2 | a3;
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    return false;
    break;

  case E_CONCAT:
    do {
      a1 = var_appears_in_expr (e->u.e.l, vx, s, v);
      e = e->u.e.r;
    } while (e && !a1);
    return a1;
    break;

  case E_BITFIELD:
    /* l is an Id */
    // v = _var_getinfo ((ActId *)e->u.e.l);
    // if ((!_shared_expr_var || !v->fcurexpr) && !v->fischan) {
    //   v->nread++;
    //   v->fcurexpr = 1;
    // }
    return false;
    break;

  case E_TRUE:
  case E_FALSE:
  case E_INT:
  case E_REAL:
    return false;
    break;

  case E_VAR:
    // _chkdynamic ((ActId *)e->u.e.l);
    // if (_isdynamic_var) {
    //   return;
    // }
    // v = _var_getinfo ((ActId *)e->u.e.l);
    // // if ((!_shared_expr_var || !v->fcurexpr) && !v->fischan) {
    // if ((!v->fcurexpr) && !v->fischan) {
    //   v->nread++;
    //   v->fcurexpr = 1;
    // }
    vix = ((ActId *)e->u.e.l)->rootVx(s);

    if (!vix->t->arrayInfo() && !vx->t->arrayInfo())
      return !strcmp(v->name,((ActId *)e->u.e.l)->getName());

    else if (vix->t->arrayInfo() && vx->t->arrayInfo())
    {
      // ((ActId *)e->u.e.l)->sPrint(str,1024,NULL,style_global);
      // a_mangle->mangle_string(str,t,1024);
       get_true_name (t, ((ActId *)e->u.e.l), s);
      return !strcmp(v->name, t);
    }

    else
      return false;
    break;

  case E_PROBE:
    fatal_error ("Not handling probes right now");
    // _chkdynamic ((ActId *)e->u.e.l);
    // if (_isdynamic_var) {
    //   return;
    // }
    // v = _var_getinfo ((ActId *)e->u.e.l);
    // if (!_shared_expr_var || !v->fcurexpr) {
    // if (!v->fcurexpr) {
    //   Assert (v->fischan, "What?");
    // #if 0      
    //       v->nread++;
    //       v->fcurexpr = 1;
    // #endif      
    // }
    return false;
    break;

  case E_BUILTIN_BOOL:
  case E_BUILTIN_INT:
    a1 = var_appears_in_expr (e->u.e.l, vx, s, v);
    return a1;
    break;
    
  case E_FUNCTION:
    warning ("not handling functions");
    return false;
    e = e->u.fn.r;
    while (e) {
      var_appears_in_expr (e->u.e.l, vx, s, v);
      e = e->u.e.r;
    }
    break;

  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    return false;
    break;
  }
}

/*
    Given a variable, goes through the chp tree and populates 
    a var_info object for that variable.
*/
var_info *construct_var_info (act_chp_lang_t *c, ValueIdx *vidx, Process *p, var_info *v, 
                              int root, char *array_suffix)
{
  // ActId *id;
  ValueIdx *vx;
  InstType *it;
  Array *a;
  ActId *id1;
  Scope *s = p->CurScope();
  char *mname;
  char str[1024];
  char t[1024];

  if (root == 1)
  {
    NEW(v, var_info);
    v->width = TypeFactory::bitWidth(vidx->t);
    v->nread = 0;
    v->iread = 0;
    v->nwrite = 0;
    v->iwrite = 0;
    v->latest_for_read = 0;
    v->latest_latch_branches = list_new();
    if (vidx->t->arrayInfo()) 
    {
      Assert (array_suffix, "huh");
      // v->array_size = vidx->t->arrayInfo()->size();
      v->array_size = 0;
      asprintf(&mname, "%s%s", vidx->getName(), array_suffix);
      v->name = mname;
    }
    else
    {
      v->array_size = 0;
      v->name = vidx->getName();
    }
  }

  int x;
  int pblock = 0;
  int changed = 0;
  act_attr *attr;

  if (!c) return v;

  switch (c->type) {

  case ACT_CHP_SKIP:
    break;

  case ACT_CHP_ASSIGN:
    // _chkdynamic (c->u.assign.id);
    // if (_isdynamic_var) {
    //   return;
    // }
    // v = _var_getinfo (c->u.assign.id);
    // id = c->u.assign.id;
    // if (id->isEqual(var_id))
    vx = (c->u.assign.id)->rootVx(s);
    if (!strcmp(vx->getName(),vidx->getName()))
    { 
      if (!array_suffix)
        v->nwrite++;
      else {
        id1 = c->u.assign.id;
        // id1->sPrint(str,1024,NULL,style_global);
        // a_mangle->mangle_string(str,t,1024);
        get_true_name (t, id1, s);
        if (!strcmp(t,v->name))
        {
          v->nwrite++;
        }
      }
    }
    // x = v->nread;
    // _clear_var_flags (v);
    if ( var_appears_in_expr (c->u.assign.e, vidx, s, v) )
      v->nread++;
    // if (x != v->nread) {
    //   c->type = ACT_CHP_ASSIGNSELF;
    // }
    break;

  case ACT_CHP_SEND:
    // _chkdynamic (c->u.comm.chan);
    // if (_isdynamic_var) {
    //   return;
    // }
    // v = _var_getinfo (c->u.comm.chan);
    if (v->fisinport == 2) {
      if (v->block_out != -1 && v->block_out != pblock) {
        warning ("Channel has multiple potentially concurrent senders?");
        fprintf (stderr, "\t Channel: ");
        // v->id->Print (stderr);
        fprintf (stderr, "\n");
      }
      v->block_out = pblock;
    }
    // v->nwrite++;
    if (c->u.comm.e) {
      if ( var_appears_in_expr (c->u.comm.e, vidx, s, v) )
        v->nread++;
    }
    break;

  case ACT_CHP_RECV:
    // v = _var_getinfo (c->u.comm.chan);
    if (c->u.comm.var) {
      // id = c->u.comm.var;
      vx = (c->u.comm.var)->rootVx(s);
      if (!strcmp(vx->getName(),vidx->getName()))
      { 
      if (!array_suffix)
        v->nwrite++;
      else {
        id1 = c->u.comm.var;
        // id1->sPrint(str,1024,NULL,style_global);
        // a_mangle->mangle_string(str,t,1024);
        get_true_name (t, id1, s);
        if (!strcmp(t,v->name))
        {
          v->nwrite++;
        }
      }
    }
    }
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    if (pblock == -1 && c->type == ACT_CHP_SEMI) {
      // _block_id = -2;
    }
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
      v = construct_var_info ((act_chp_lang_t *) list_value (li), vidx, p, v, 0, array_suffix);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_DOLOOP:
    {
      act_chp_gc_t *gc = c->u.gc;
      /* group all guard variables together */
      // _clear_var_flags ();
      while (gc) 
      {
        if (gc->g) {
          if ( var_appears_in_expr (gc->g, vidx, s, v) )
            v->nread++;
        }
        gc = gc->next;
      }
      /* handle statements */
      gc = c->u.gc;
      while (gc) {
      // _clear_var_flags ();
      v = construct_var_info (gc->s, vidx, p, v, 0, array_suffix);
      gc = gc->next;
      }
    }
    break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return v;
}

/*
    Construct a hashtable that contains the var_info objects for
    all the variables in a given process. 
*/
Hashtable *construct_var_info_hashtable (act_chp_lang_t *c, Process *p)
{
  Hashtable *var_infos = hash_new(4);
  hash_bucket_t *b;
  var_info *v;
  ActId *id;
  Array *a;
  Arraystep *as;
  ActInstiter iter(p->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    // if (!TypeFactory::isChanType (vx->t) && !vx->t->arrayInfo()) 
    if (TypeFactory::isDataType (vx->t) && !vx->t->arrayInfo()) 
    {
      v = construct_var_info (c, vx, p, v, 1, NULL);
      b = hash_add (var_infos, v->name);
      b->v = v;
    }
    // else if (!TypeFactory::isChanType (vx->t))
    else if (TypeFactory::isDataType (vx->t))
    {
      a = vx->t->arrayInfo();
      as = a->stepper();
      while (!as->isend()) 
      {
        // int idx = as->index(); // the index of the element
        Array *elem = as->toArray(); // the array de-reference for the element
        char t[1024];
        generate_array_suffix (t, elem);
        v = construct_var_info (c, vx, p, v, 1, t);
        b = hash_add (var_infos, v->name);
        b->v = v;
        as->step(); // advance the stepper
      }
    }
    else
    {
      
    }
  }
  return var_infos;
}

/*
    Print the current state of the hashtable of var_info objects
*/
Hashtable *print_var_info_hashtable (Hashtable *var_infos)
{
  hash_bucket_t *b;
  var_info *v;
  int i;

  for (i = 0; i < var_infos->size; i++)
    for (b = var_infos->head[i]; b; b = b->next) 
    {
      print_var_info (stdout, (var_info *)b->v);
    }	     
  return var_infos;
}
