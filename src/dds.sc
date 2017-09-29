object DDS {
  // problem 1
  def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
    if (halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)
  x
}