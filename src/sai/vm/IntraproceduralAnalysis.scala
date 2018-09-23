package vm

import bytecode.BasicBlock
import cg.ConnectionGraph
import implicits.MutableSetExtensions.convert

object IntraproceduralAnalysis {

  def apply(initialFrame: Frame,
            controlFlowGraph: List[BasicBlock],
            findSuccessors: BasicBlock => List[BasicBlock],
            findPredecessors: BasicBlock => List[BasicBlock]): ConnectionGraph = {

    // The calculation of the summary information starts with the first basic block in the control flow graph.
    val entryBlock = controlFlowGraph.head
    // We use a worklist in which we store we still need to interpret.
    val worklist = scala.collection.mutable.Set(entryBlock)

    // We store all output frames of each interpreted basic block.
    val outputFrames = scala.collection.mutable.Map.empty[BasicBlock, Set[Frame]]

    // A basic block is interpreted a maximum of 'threshold' times.
    // The algorithm terminates prematurely if the limit for a block has been reached.
    val iterations       = scala.collection.mutable.Map.empty[BasicBlock, Int]
    val threshold        = 10
    var reachedThreshold = false

    while (worklist.nonEmpty && !reachedThreshold) {
      // Pick and remove any block from the worklist.
      val currentBlock = worklist.removeArbitrary()

      val inputFrames = findPredecessors(currentBlock) match {
        case Nil => Set(initialFrame)
        case ps  => ps.flatMap(outputFrames.getOrElse(_, Set.empty)).toSet
      }

      // Merge connection graphs of each ingoing frame.
      val inState = inputFrames.map(_.cg).reduce(_ merge _)

      // Interpret each input frame.
      val interpretedFrames = for {
        inputFrame       <- inputFrames
        frameToInterpret = inputFrame.copy(cg = inState)
        interpretedFrame <- currentBlock.interpret(frameToInterpret)
      } yield interpretedFrame

      // There is a change if the output frames before the interpretation are different from those after the interpretation.
      val framesChanged = outputFrames.get(currentBlock) != Some(interpretedFrames)
      if (framesChanged) {
        // Store the interpreted frames as output frames for the current block.
        outputFrames(currentBlock) = interpretedFrames
        // Add all successor blocks to the worklist since they may also change in the next iteration.
        worklist ++= findSuccessors(currentBlock)
        // Increment the iteration counter for the block.
        iterations(currentBlock) = iterations.getOrElse(currentBlock, 0) + 1
        // Check if we reached the threshold for the current block.
        reachedThreshold = iterations(currentBlock) == threshold
      }
    }

    if (reachedThreshold) {
      // If we reached the threshold, then we use the bottom solution (i.e. mark all object nodes as global escape)
      val summary = outputFrames.values.flatten.map(_.cg).reduce(_ merge _)
      summary.bottomSolution
    } else {
      // If we did not reach the threshold, we perform the reachability analysis in order to update the escape states of the nodes.
      val summary = outputFrames(controlFlowGraph.last).map(_.cg).reduce(_ merge _)
      summary.performReachabilityAnalysis
    }
  }

}
