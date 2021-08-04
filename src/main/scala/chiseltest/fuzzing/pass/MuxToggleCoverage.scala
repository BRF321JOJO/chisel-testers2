// Copyright 2017-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Jack Koenig <koenig@sifive.com>, Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.fuzzing.pass

import chiseltest.fuzzing.annotations.DoNotCoverAnnotation
import firrtl._
import firrtl.annotations._
import firrtl.options.Dependency

import scala.collection.mutable

// adds mux toggle coverage with a coverage statement
// see: https://people.eecs.berkeley.edu/~laeufer/papers/rfuzz_kevin_laeufer_iccad2018.pdf
// TODO: this transform should build upon the standard toggle coverage pass once that is published + polished!
object MuxToggleCoverage extends Transform with DependencyAPIMigration {
  override def prerequisites = Seq(
    Dependency[firrtl.transforms.RemoveWires], Dependency(passes.ExpandWhens), Dependency(passes.LowerTypes)
  )
  override def invalidates(a: Transform) = false

  override def execute(state: CircuitState): CircuitState = {
    val c = CircuitTarget(state.circuit.main)
    val newAnnos = mutable.ListBuffer[Annotation]()
    val circuit = state.circuit.mapModule(onModule(_, collectModulesToIgnore(state), c, newAnnos))
    //println(circuit.serialize)
    state.copy(circuit = circuit, annotations = newAnnos.toList ++: state.annotations)
  }

  private def onModule(m: ir.DefModule, ignoreSet: Set[String], c: CircuitTarget, newAnnos: mutable.ListBuffer[Annotation]): ir.DefModule = m match {
    case mod: ir.Module if !ignoreSet.contains(mod.name) =>
      val ctx = ModuleCtx(c.module(mod.name), Namespace(mod), newAnnos, findClock(mod), findReset(mod))
      val conds = findMuxConditions(mod)
      val (stmts, annos) = coverFullToggle(ctx, conds)
      assert(annos.isEmpty)
      mod.copy(body = ir.Block(mod.body +: stmts))
    case other => other
  }

  def collectModulesToIgnore(state: CircuitState): Set[String] = {
    val main = state.circuit.main
    state.annotations.collect { case DoNotCoverAnnotation(target) if target.circuit == main => target.module }.toSet
  }

  // TODO: replace with library function
  private def findClock(m: ir.Module): ir.Expression = {
    m.ports.collectFirst { case p @ ir.Port(_, _, ir.Input, ir.ClockType) => ir.Reference(p) }.getOrElse(
      throw new RuntimeException(s"Couldn't find a clock input for:\n${m.serialize}")
    )
  }

  // TODO: replace with library function
  private def findReset(m: ir.Module): ir.Expression = {
    m.ports.find(_.name == "reset").map(p => ir.Reference(p)).getOrElse(
      throw new RuntimeException(s"Couldn't find a reset input for:\n${m.serialize}")
    )
  }

  private case class ModuleCtx(m: ModuleTarget, namespace: Namespace, newAnnos: mutable.ListBuffer[Annotation],
    clock: ir.Expression, reset: ir.Expression)

  private def coverToggle(ctx: ModuleCtx, conds: List[ir.Expression]): (List[ir.Statement], List[Annotation]) = {
    val prev_reset_reg = ir.DefRegister(ir.NoInfo, ctx.namespace.newName("prev_reset"), Utils.BoolType,
      ctx.clock, Utils.zero, Utils.zero)
    val prev_reset_ref = ir.Reference(prev_reset_reg)
    val prev_reset_connect = ir.Connect(ir.NoInfo, prev_reset_ref, ctx.reset)

    val stmts: List[ir.Statement] = conds.flatMap { muxCond =>
      val name: String = muxCond match {
        case ir.Reference(name, _, _, _) => name
        case _ => "mux_cond"
      }
      val cond = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_s"), muxCond)
      val condRef = ir.Reference(cond)

      val prev_cond_reg = ir.DefRegister(ir.NoInfo, ctx.namespace.newName(name + "_prev"), Utils.BoolType,
        ctx.clock, Utils.zero, Utils.zero)
      val prev_cond_ref = ir.Reference(prev_cond_reg)
      val prev_cond_connect = ir.Connect(ir.NoInfo, prev_cond_ref, condRef)

      val toggle = ir.DoPrim(PrimOps.Xor, Seq(condRef, prev_cond_ref), Seq.empty, Utils.BoolType)
      val toggle_node = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_toggle"), toggle)

      val toggleNoReset = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_toggleNoReset"),
        Utils.and(ir.Reference(toggle_node), Utils.not(Utils.or(ctx.reset, prev_reset_ref))))
      val toggleNoResetCov = ir.Verification(ir.Formal.Cover, ir.NoInfo, ctx.clock, ir.Reference(toggleNoReset),
        Utils.one, ir.StringLit(""), ctx.namespace.newName(name + "_toggleNoResetCov"))

      List(cond, prev_cond_reg, prev_cond_connect, toggle_node, toggleNoReset, toggleNoResetCov)
    }
    (prev_reset_reg :: prev_reset_connect :: stmts, List())
  }

  // returns a list of unique (at least structurally unique!) mux conditions used in the module
  private def findMuxConditions(m: ir.Module): List[ir.Expression] = {
    val conds = mutable.LinkedHashMap[String, ir.Expression]()

    def onStmt(s: ir.Statement): Unit = s match {
      case ir.Block(stmts) => stmts.foreach(onStmt)
      case other => other.foreachExpr(onExpr)
    }
    def onExpr(e: ir.Expression): Unit = {
      e.foreachExpr(onExpr)
      e match {
        case ir.Mux(cond, _, _, _) =>
          val key = cond.serialize
          conds(key) = cond
        case _ =>
      }
    }
    onStmt(m.body)
    conds.values.toList
  }

  private def coverFullToggle(ctx: ModuleCtx, conds: List[ir.Expression]): (List[ir.Statement], List[Annotation]) = {
    val prev_reset_reg = ir.DefRegister(ir.NoInfo, ctx.namespace.newName("prev_reset"), Utils.BoolType,
      ctx.clock, Utils.zero, Utils.zero)
    val prev_reset_ref = ir.Reference(prev_reset_reg)
    val prev_reset_connect = ir.Connect(ir.NoInfo, prev_reset_ref, ctx.reset)

    val stmts: List[ir.Statement] = conds.flatMap { muxCond =>
      val name: String = muxCond match {
        case ir.Reference(name, _, _, _) => name
        case _ => "mux_cond"
      }
      val cond = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_s"), muxCond)
      val condRef = ir.Reference(cond)

      val prev_cond_reg = ir.DefRegister(ir.NoInfo, ctx.namespace.newName(name + "_prev"), Utils.BoolType,
        ctx.clock, Utils.zero, Utils.zero)
      val prev_cond_ref = ir.Reference(prev_cond_reg)
      val prev_cond_connect = ir.Connect(ir.NoInfo, prev_cond_ref, condRef)

      val toggle = ir.DoPrim(PrimOps.Xor, Seq(condRef, prev_cond_ref), Seq.empty, Utils.BoolType)
      val toggle_node = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_toggle"), toggle)

      val toggleNoReset = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_toggleNoReset"),
        Utils.and(ir.Reference(toggle_node), Utils.not(Utils.or(ctx.reset, prev_reset_ref))))
      val toggleNoReset_ref = ir.Reference(toggleNoReset)

      // toggleStore saves as a 1 when a 0-1 or 1-0 toggle has been seen before, but waiting for a "full" toggle.
      val toggleStore_reg = ir.DefRegister(ir.NoInfo, ctx.namespace.newName(name + "_toggleStore"), Utils.BoolType, ctx.clock, Utils.zero, Utils.zero)
      val toggleStore_ref = ir.Reference(toggleStore_reg)
      //This switch logic will cause toggleStore to update (toggle) from 0 to 1 or 1 or 0.
      val toggleStore_switch = ir.DoPrim(PrimOps.Xor, Seq(toggleStore_ref, toggleNoReset_ref), Seq.empty, Utils.BoolType)
      val toggleStore_connect = ir.Connect(ir.NoInfo, toggleStore_ref, toggleStore_switch)

      // fullToggle is a 1 on cycles when there is a new toggle and a toggle has been seen before, saved in toggleStore
      val fullToggle = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_fullToggleNoReset"), Utils.and(toggleStore_ref, toggleNoReset_ref))
      val fullToggleCov = ir.Verification(ir.Formal.Cover, ir.NoInfo, ctx.clock, ir.Reference(fullToggle), Utils.one, ir.StringLit(""), ctx.namespace.newName(name + "_fullToggleNoResetCov"))

      List(cond, prev_cond_reg, prev_cond_connect, toggle_node, toggleNoReset, toggleStore_reg, toggleStore_connect, fullToggle, fullToggleCov)
    }
    (prev_reset_reg :: prev_reset_connect :: stmts, List())
  }

  private def coverPseudoToggle(ctx: ModuleCtx, conds: List[ir.Expression]): (List[ir.Statement], List[Annotation]) = {
    val stmts = conds.flatMap { cond =>
      val name = cond match {
        case ir.Reference(name, _, _, _) => name
        case _ => "mux_cond"
      }
      val node = ir.DefNode(ir.NoInfo, ctx.namespace.newName(name + "_s"), cond)
      val nodeRef = ir.Reference(node)
      val oneCover = ir.Verification(ir.Formal.Cover, ir.NoInfo, ctx.clock, nodeRef, Utils.not(ctx.reset),
        ir.StringLit(""), ctx.namespace.newName(name + "_one"))
      val zeroCover = ir.Verification(ir.Formal.Cover, ir.NoInfo, ctx.clock, Utils.not(nodeRef), Utils.not(ctx.reset),
        ir.StringLit(""), ctx.namespace.newName(name + "_zero"))
      List(node, oneCover, zeroCover)
    }
    (stmts, List())
  }


}
