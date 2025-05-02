/* Copyright (C) 2012,2019-2020 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

// #if ASSERT
// static bool invariantForGC (GC_state s);
// #endif

static inline bool invariantForMutatorFrontier (GC_state s);
static inline bool invariantForMutatorStack (GC_state s);
#if ASSERT
static bool invariantForMutator (GC_state s, bool frontier, bool stack);
bool carefulInvariantForMutatorStack(GC_state s);
void displayStackInfo(GC_state s);
#endif

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
