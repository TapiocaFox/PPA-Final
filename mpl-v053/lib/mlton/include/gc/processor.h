#include "gc_state.h"

#ifndef PROCESSOR_H_
#define PROCESSOR_H_

#if (defined (MLTON_GC_INTERNAL_BASIS))
/*************/
/* Interface */
/*************/
/* Unique number for this thread */
int32_t Proc_processorNumber (GC_state s);

/* Used to make sure all threads are properly initialized */
void Proc_waitForInitialization (GC_state s);
void Proc_signalInitialization (GC_state s);
bool Proc_isInitialized (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

#endif /* PROCESSOR_H_ */
