#ifndef SYNTHESIS_STRUCT_H
#define SYNTHESIS_STRUCT_H

/*
    Information about each variable in a process w.r.t. its 
    use in the CHP body. Fields are quite self-explanatory.
*/
struct var_info {
  // ActId *id;
  const char *name;

  /*-- flags --*/
  unsigned int fcurexpr:1;	// found in current expression
  unsigned int fischan:1;	// channel or int?
  unsigned int fisinport:2;	// 1 if input, 0 if output, 2 if both
  unsigned int fisbool:1;	// bool variable or bool chan

  int width;			// bitwidth

  int block_in, block_out;	// for internal channels

  int latest_for_read; // correct id for reading from (used for getting data inside branches correctly)

  int nread, nwrite;		// for variables
                                //     nread  = total # of reads
                                //     nwrite = total # of writes
				// for channels
                                //     nread  = total # of receives
                                //     nwrite = total # of sends
  
  int iread, iwrite;		// running counter used for muxing ( aka choosing the latch :) )

  list_t *latest_latch_branches;

  int array_size;

};

/*
    Not used yet. Will be used to determine if a given CHP
    program is of the form that can be sequenced with a
    single pipeline element.
*/
struct chp_form_info {

  int valid_dag;
  
  int dag_length;

  int dag_form;
  
};

#endif