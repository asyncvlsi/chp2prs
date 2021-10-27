#include "externoptsdt.h"
#include <algorithm>
#include <string.h>


/* id = expr_id for evaluating this expression */
void ExternOptSDT::_emit_expr (int *id, int tgt_width, Expr *e)
{
  int width = 0;
  int myid;
  // a list of all the inputs
  list_t *all_leaves;
  listitem_t *li;
  // the metadata item given back by the syntesis tool.
  ExprBlockInfo *block_info;

  /*-- recursively expand expression --*/
  if (!e) {
    fatal_error ("Emit NULL expression?!");
  }

  if (!_efp) {
    fatal_error ("ExternOptSDT: requires block expression mode");
  }
  
  if (!mapper) {
    mapper = new ExternalExprOpt(_map,
				 bundled_data ? bd : qdi,
				 bundled_data ? true : false,
				 _exprfile, "e", "blk");
  }
  Assert (mapper, "Could not create mapper!");

  // make sure the maps are empty and create new ones
  //Assert (!_inexprmap, "What?");
  _inexprmap = ihash_new (0);
  _inwidthmap = ihash_new (0);

  // run through the expr data structure and collect all variables and constants
  _expr_collect_vars (e, 1);

  // fill up all the leaves list with all the elements in the inexprmap
  all_leaves = list_new ();
  {
    ihash_iter_t iter;
    ihash_bucket_t *ib;
    ihash_iter_init (_inexprmap, &iter);
    while ((ib = ihash_iter_next (_inexprmap, &iter))) {
      Expr *e = (Expr *)ib->key;
      list_append (all_leaves, e);
    }
  }



  // generate block id and check if we have a expression file to write the blocks too
  int xid = _gen_expr_blk_id ();
  if (!_efp)
  {
    fatal_error("need expr file for optimisation mode");
  }

  /*-- emit leaves --*/
  _expr_collect_vars (e, 0);

  //convert std expr channel to array of expr channels
  if (bundled_data != 1) 
  {
    for (li = list_first (all_leaves); li; li = list_next (li))
    {
      ihash_bucket_t *bucket_id = ihash_lookup(_inexprmap, (long) list_value (li));
      ihash_bucket_t *bucket_w = ihash_lookup(_inwidthmap, (long) list_value (li));
      if (bucket_w->i > 1){
        int new_id = _gen_expr_id();
        _emit_expr_array_wrap(new_id, bucket_id->i, bucket_w->i);
        bucket_id->i = new_id;
      }
    }
  }

  fclose (_efp);

  /*-- emit expression --*/
  block_info = mapper->run_external_opt(xid,tgt_width,e,all_leaves,_inexprmap,_inwidthmap);

  _efp = fopen (_exprfile, "a");

  // the current id of the output instance
  *id = _gen_expr_id ();
  // instanciate the expr block object in the main file
  {
    listitem_t *li;
    list_t *ids;
    ihash_iter_t iter;
    ihash_bucket_t *ib; 
    ids = list_new ();
    
    _emit_expr_block (*id, xid, all_leaves);
  }
  // do the conversion from wires/arraychannels to channels and for BD wrap the ctl arround
  fprintf (output_stream,"   // wrap and ctl bypass\n");
  int number_of_leaves = 0;
  /*-- wrapping only  -  width conversion is done by the syntesis --*/
  myid = _gen_expr_id ();
  _emit_expr_width_conv (*id, width, myid, tgt_width);

  *id = myid;
  /*-- handshake bypass --*/
  if (bundled_data == 1) 
  {
    double delay;

    delay = block_info->delay_max;
    if (delay == 0) {
      delay = block_info->delay_typ;
    }
    if (delay == 0) {
      delay = block_info->delay_min;
    }
    _emit_bd_ctl_bypass (*id, all_leaves, delay);
  }
  fprintf (output_stream,"   //end expr blk%u\n",xid);

  // free all temporary data structures 
  ihash_free (_inexprmap);
  _inexprmap = NULL;
  ihash_free (_inwidthmap);
  _inwidthmap = NULL;
  list_free (all_leaves);
  // force write output file
  fflush(output_stream);
}

void ExternOptSDT::_emit_bd_ctl_bypass (int id, list_t *all_leaves, double delay_max)
{
  // distribute the request signal off the pull channel
  // in case of BD connect all the ack signals via a c element tree
  listitem_t *li;
  int number_of_leaves = 0;
  for (li = list_first (all_leaves); li; li = list_next (li))
  {
    fprintf(output_stream, "   e%u.out.r = e%u.out.r;\n", ihash_lookup(_inexprmap, (long) list_value (li))->i, id);
    number_of_leaves++;
  }
  if (number_of_leaves > 0) 
  {
    int index = 0;
    fprintf(output_stream, "   syn::ctree<%u,false> ackmerge%u;\n", number_of_leaves, id);
    fprintf (output_stream, "   /* delay: %g */\n", delay_max);
    fprintf(output_stream, "   syn::delay<50> delayblk%u (ackmerge%u.out, e%u.out.a);\n", id, id, id);
    for (li = list_first (all_leaves); li; li = list_next (li))
    {
      fprintf(output_stream, "   e%u.out.a = ackmerge%u.in[%u];\n", ihash_lookup(_inexprmap, (long) list_value (li))->i, id, index);
      index++;
    }
  }
  else if (number_of_leaves == 0)
  {
    fprintf(output_stream, "   e%u.out.a = e%u.out.r;\n", id, id);
  }
}

void ExternOptSDT::_emit_expr_block (int id, int blkid, list_t *exprs)
{
  // instanciate the expr block in the main file and connect all the input signals
  listitem_t *li;
  fprintf (output_stream, "   syn::expr::blk%d e%d(", blkid, id);
  // each element in this list is a input expr object, the map is uesd to look up the coresponding ID.
  for (li = list_first (exprs); li; li = list_next (li)) {
    if (li != list_first (exprs)) {
      fprintf (output_stream, ", ");
    }
    // qdi and bd have different channel and wire names
    if (bundled_data != 1) fprintf (output_stream, "e%d.out", ihash_lookup(_inexprmap, (long) list_value (li))->i);
    else
    {
       fprintf (output_stream, "e%d.out.d", ihash_lookup(_inexprmap, (long) list_value (li))->i);
      // if the signal is 1 bit wide provide the actuall wire not the array
      if (ihash_lookup(_inwidthmap, (long) list_value (li))->i == 1) fprintf (output_stream, "[0]");
    }
  }
  fprintf (output_stream, ");\n");
}

void ExternOptSDT::_emit_expr_array_wrap(int new_id, int old_id, int width)
{
  if (width > 1 ) {
    fprintf (output_stream, "   syn::expr::wrap_to_array<%d> e%d(e%d.out);\n", width, new_id, old_id); 
  } 
  else 
  { // nothing todo is already in correct form, should not go here should be handled idealy before, but if not this creates and alias
    fprintf(output_stream, "   syn::expr::null e%u(e%u.out);\n", new_id, old_id);
  }
}

void ExternOptSDT::_emit_expr_width_conv (int from, int from_w, int to, int to_w)
{
  //for bundled data from_w is 0, for qdi now too, width conversion is heandled by syntesis tool
  if (bundled_data == 1)
  {
    // catch the case of the single bit wire, dont use an array in this case provide the acctual wire
    if (to_w == 1) 
    {
      fprintf(output_stream, "   syn::expr::null e%u();\n", to);
      fprintf(output_stream, "   e%u.out.d[0]=e%u.out;\n", to, from);
    }
    else 
       {
      fprintf(output_stream, "   syn::expr::nullint<%u> e%u();\n", to_w, to);
      fprintf(output_stream, "   e%u.out.d=e%u.out;\n", to, from);
    }

  }
  else
  {
    // catch width 1 because output is than not an array but a single channel that is fine
    if (to_w > 1 ) {
      fprintf (output_stream, "   syn::expr::wrap_from_array<%d> e%d(e%d.out);\n", to_w, to, from); 
    }
    else 
    { // nothing todo is already in correct form, should not go here should be handled idealy before, but if yes this creates and alias
      fprintf(output_stream, "   syn::expr::null e%u(e%u.out);\n", to, from);
    }
  }
}

int ExternOptSDT::get_expr_width(Expr *ex) {
  // recusivly run through the expression and collect its width
  switch ((ex)->type)
  {
  // for a var read the bitwidth of that var
  case E_VAR:
  {
    varmap_info *v;  
    v = _var_getinfo ((ActId *)(ex)->u.e.l);
    return v->width;
  }
  // for true and false the bit width is one
  case E_TRUE:
  case E_FALSE:
    return 1;
  // for int look up the corresponding bitwidth
  case E_INT:
  {
    return ihash_lookup(_inwidthmap, (long) ex)->i;
  }
  // step through
  case E_QUERY:
    ex = ex->u.e.r;
  // get the max out of the right and the left expr part
  case E_AND:
  case E_OR:
  case E_XOR:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return std::max(lw,rw);
  }
  // get the max out of the right and the left expr part and one for the overflow bit
  case E_PLUS:
  case E_MINUS:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return std::max(lw,rw);
    // @TODO genus trys to tie the top bit and i dont know why
    // return std::max(lw,rw)+1;
  }
  // comparisons result in a bool so 1, do not walk further
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
    // should be fine in ignoring the rest of the expr
    return 1;
  // for multiplication add both operand bitwidth
  case E_MULT:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return lw+rw;
  } 
  // step through
  case E_MOD:
  {
    int rw = get_expr_width(ex->u.e.r);
    return rw;
  } 
  // use left bitwidth and add number of shifted right
  case E_LSL:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return lw + (1 << rw);
  }
  // pass through
  case E_DIV:
  case E_LSR:
  case E_ASR:
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
  {
    int lw = get_expr_width(ex->u.e.l);
    return lw;
  }  

  //get the value out of the datastructure
  case E_BUILTIN_INT:
    if (ex->u.e.r) {
      Assert (ex->u.e.r->type == E_INT, "What?");
      return ex->u.e.r->u.v;
    }
    else {
      return 1;
    }

  case E_BUILTIN_BOOL:
    return 1;
  // the following ones should give you errors because not handled
  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    fatal_error ("fix concat");
    break;

  case E_BITFIELD:
    fatal_error ("fix bitfield");
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_PROBE:
    fatal_error ("fix probes please");
    break;
    
  case E_FUNCTION:
    fatal_error ("function!");
    
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", ex->type);
    break;
  }
  return 0;
}

void ExternOptSDT::_emit_expr_const (int id, int width, int val)
{
  // some how overwriting and optional overload dont work together
  _emit_expr_const (id, width, val, false);
}

void ExternOptSDT::_emit_expr_const (int id, int width, int val, bool isguard)
{
  // for QDI emmit constant
  if (bundled_data != 1) fprintf (output_stream, "   syn::expr::const<%d,%d> e%d;\n", width, val, id);
  // for BD emmit only if its a guard for eg infinit loop
  else if (isguard) fprintf (output_stream, "   syn::trueto1of2 e%d;\n", id);
  // print for debugging BD constants, they should be handed to the syntesis tool
  else fprintf (output_stream, "   // would emit const e%d <%d> %d %d\n", id, width,val,isguard);
}

void ExternOptSDT::_emit_guardlist (int isloop, act_chp_gc_t *gc, list_t *res)
{
  act_chp_gc_t *tmp;
  int eid;
  Assert (gc, "Why am I here?");
  // if it is a infinit loop indicate that to the const gen so its not ommited for BD
  if (isloop && !gc->g) {
    /*-- infinite loop --*/
    eid = _gen_expr_id ();
    _emit_expr_const (eid, 1, 1, true);
    list_iappend (res, eid);
  }
  //run through all guards, @TODO rewrite guard handling to combine them into one optimisation block
  // that needs also new control structures
  else {
    tmp = gc;
    while (tmp) {
      if (tmp->g) {
        _emit_expr (&eid, 1, tmp->g);
        eid = _gen_safe_bool (eid);
        list_iappend (res, eid);
      }
      else {
        eid = -1;
        list_iappend (res, eid);
      }
      tmp = tmp->next;
    }
  }
}

/* 
   returns new eid for the safe bool
*/
int ExternOptSDT::_gen_safe_bool (int eid)
{
  ActId *id;

  Assert (_gen_fresh_var (1, &id), "What?");

  /*
    Sequence:
      1. transfer expression to variable
      2. read variable into expression
  */
  int tid = _gen_stmt_id ();
  _emit_transfer (tid, eid, id);

  int fid = _gen_stmt_id ();
  _emit_trueseq (fid, tid);

  eid = _gen_expr_id ();
  _emit_var_read (eid, id);

  int eid2 = _gen_expr_id ();
  

  if (bundled_data != 1)
  {
    fprintf (output_stream, "   syn::expr::null e%d;\n", eid2);
    fprintf (output_stream, "   e%d.out.r = c%d.r; c%d.a = e%d.out.r; e%d.out.d=e%d.out.d;\n",
     eid2, fid, fid, eid, eid2, eid);
  }
  else
  {
    fprintf (output_stream, "   syn::bdto1of2 e%d;\n", eid2);
    fprintf (output_stream, "   e%d.out.r = c%d.r; c%d.a = e%d.out.r; e%d.out.d=e%d.out.d; e%d.out.a=e%d.out.a;\n",
     eid2, fid, fid, eid, eid2, eid, eid2, eid);
  }
  return eid2;
}

void ExternOptSDT::_expr_collect_vars (Expr *e, int collect_phase)
{
  int id;

  Assert (e, "Hmm");

#define BINARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, collect_phase);	\
    _expr_collect_vars (e->u.e.r, collect_phase);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, collect_phase);	\
  } while (0)
  
  switch (e->type) {
    /* binary */
  case E_AND:
  case E_OR:
  case E_XOR:
  case E_PLUS:
  case E_MINUS:
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
  case E_MULT:
  case E_DIV:
  case E_MOD:
  case E_LSL:
  case E_LSR:
  case E_ASR:
    BINARY_OP;
    break;
    
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
  case E_BUILTIN_INT:
  case E_BUILTIN_BOOL:
    UNARY_OP;
    break;

  case E_QUERY:
    e = e->u.e.r;
    BINARY_OP;
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    fatal_error ("fix concat");
    break;

  case E_BITFIELD:
    fatal_error ("fix bitfield");
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_TRUE:
    if (0) {
      if (collect_phase) {
        ihash_bucket_t *b_name;
        b_name = ihash_add (_inexprmap, (long) e);
        b_name->i = _gen_expr_id ();
        ihash_bucket_t *b_width;
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = 1;

      }
      else {
        id = ihash_lookup(_inexprmap, (long) e)->i;
        _emit_expr_const (id, 1, 1);
      }
    }
    
    break;
    
  case E_FALSE:
    if (!bundled_data) {
      if (0) {
        ihash_bucket_t *b_name;
        b_name = ihash_add (_inexprmap, (long) e);
        b_name->i = _gen_expr_id ();
        ihash_bucket_t *b_width;
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = 1;
      }
      else {
        id = ihash_lookup(_inexprmap, (long) e)->i;
        _emit_expr_const (id, 1, 0);
      }
    }
    break;
    
  case E_INT:
    if (0) {
      if (collect_phase) {
        int w = 0;
        int val = e->u.v;
        if (val < 0) {
          val = -val;
          w = 32;
        }
        else {
          while (val) {
            val >>= 1;
            w++;
          }
        }
        if (w == 0) {
          w = 1;
        }
        ihash_bucket_t *b_name;
        b_name = ihash_add (_inexprmap, (long) e);
        b_name->i = _gen_expr_id ();
        ihash_bucket_t *b_width;
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = w;
      }
      else {
        int w;
        id = ihash_lookup(_inexprmap, (long) e)->i;
        w = ihash_lookup(_inwidthmap, (long) e)->i;
        _emit_expr_const (id, w, e->u.v);
      }
    }
    break;

  case E_VAR:
    if (collect_phase) {
      varmap_info *v;
      ihash_bucket_t *b;
      v = _var_getinfo ((ActId *)e->u.e.l);
      b = ihash_add (_inexprmap, (long)e);
      b->i = _gen_expr_id ();
      ihash_bucket_t *b_width;
      b_width = ihash_add (_inwidthmap, (long) e);
      b_width->i = v->width;
    }
    else {
      ihash_bucket_t *b;
      b = ihash_lookup (_inexprmap, (long)e);
      _emit_var_read (b->i, (ActId *)e->u.e.l);
    }
    break;

  case E_PROBE:
    fatal_error ("fix probes please");
    break;
    
  case E_FUNCTION:
    fatal_error ("function!");
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
  }
  return;
#undef BINARY_OP
#undef UNARY_OP
}
