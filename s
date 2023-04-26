[1mdiff --git a/modules/core/src/main/scala/turbolift/internals/engine/FiberImpl.scala b/modules/core/src/main/scala/turbolift/internals/engine/FiberImpl.scala[m
[1mindex 26fa44d3..2ed2d3c4 100644[m
[1m--- a/modules/core/src/main/scala/turbolift/internals/engine/FiberImpl.scala[m
[1m+++ b/modules/core/src/main/scala/turbolift/internals/engine/FiberImpl.scala[m
[36m@@ -60,7 +60,7 @@[m [mprivate[turbolift] final class FiberImpl private ([m
     assert(isSuspended)[m
     suspendedConfig.nn.executor.offer(this)[m
 [m
[31m-  def run(): FiberImpl | Null =[m
[32m+[m[32m  def run(): FiberImpl =[m
     assert(isSuspended)[m
     suspendedTick = suspendedConfig.nn.tickLow[m
     try[m
[36m@@ -71,11 +71,12 @@[m [mprivate[turbolift] final class FiberImpl private ([m
           if e.isInstanceOf[Panic][m
           then e[m
           else new Panic(s"Unhandled exception: ${e.getMessage}", e)[m
[31m-        findRoot.setFailure(e2)[m
[31m-        null[m
[32m+[m[32m        val that = findRoot[m
[32m+[m[32m        that.setFailure(e2)[m
[32m+[m[32m        that[m
 [m
         [m
[31m-  @tailrec private def outerLoop(tickHigh: Short): FiberImpl | Null =[m
[32m+[m[32m  @tailrec private def outerLoop(tickHigh: Short): FiberImpl =[m
     if unsafeIsCancelled() then[m
       suspendedTag = Tags.Step_Abort[m
       suspendedPayload = Cancelled[m
[36m@@ -108,7 +109,7 @@[m [mprivate[turbolift] final class FiberImpl private ([m
       kont0   = new Kont,[m
     )[m
 [m
[31m-    if that != null then[m
[32m+[m[32m    if that.isPending then[m
       if that.suspendedTick > 0 then[m
         that.outerLoop(tickHigh)[m
       else[m
[36m@@ -118,7 +119,7 @@[m [mprivate[turbolift] final class FiberImpl private ([m
         else[m
           that[m
     else[m
[31m-      null[m
[32m+[m[32m      that[m
 [m
   //===================================================================[m
   // Inner Loop[m
[36m@@ -133,7 +134,7 @@[m [mprivate[turbolift] final class FiberImpl private ([m
     store: Store,[m
     lookup: Lookup,[m
     kont0: Kont,[m
[31m-  ): FiberImpl | Null =[m
[32m+[m[32m  ): FiberImpl =[m
     if tick > 0 then[m
       val tick2 = (tick - 1).toShort[m
       (tag: @switch) match[m
[36m@@ -323,12 +324,12 @@[m [mprivate[turbolift] final class FiberImpl private ([m
                   step.tag match[m
                     case Tags.Step_Done =>[m
                       setSuccess(payload)[m
[31m-                      null[m
[32m+[m[32m                      this[m
 [m
                     case Tags.Step_Abort =>[m
                       assert(step.isGlobalAbort)[m
                       setFailure(payload)[m
[31m-                      null[m
[32m+[m[32m                      this[m
 [m
                 case Bits.Tree_Zip =>[m
                   this.suspendedPayload = payload[m
[36m@@ -404,6 +405,8 @@[m [mprivate[turbolift] final class FiberImpl private ([m
   private def setSuccess(a: Any): Unit = setResult(a, Bits.Succeeded)[m
   private def setFailure(a: Any): Unit = setResult(a, Bits.Failed)[m
 [m
[32m+[m[32m  private def isPending: Boolean = (varyingBits & (Bits.Succeeded | Bits.Failed)) == 0[m
[32m+[m
   private def setResult(a: Any, bits: Int): Unit = [m
     synchronized {[m
       suspendedPayload = a[m
[36m@@ -413,7 +416,7 @@[m [mprivate[turbolift] final class FiberImpl private ([m
 [m
   def doWait(): Unit =[m
     synchronized {[m
[31m-      if (varyingBits & (Bits.Succeeded | Bits.Failed)) == 0 then[m
[32m+[m[32m      if isPending then[m
         wait()[m
     }[m
 [m
[1mdiff --git a/modules/core/src/main/scala/turbolift/internals/engine/Misc.scala b/modules/core/src/main/scala/turbolift/internals/engine/Misc.scala[m
[1mindex 0961fb9b..38b4a6e0 100644[m
[1m--- a/modules/core/src/main/scala/turbolift/internals/engine/Misc.scala[m
[1m+++ b/modules/core/src/main/scala/turbolift/internals/engine/Misc.scala[m
[36m@@ -9,3 +9,9 @@[m [mprivate[internals] final class Panic(m: String, c: Throwable | Null = null) exte[m
 [m
 private[internals] case object Cancelled extends Throwable("Fiber cancelled", null, false, false):[m
   override def setStackTrace(x: Array[StackTraceElement]): Unit = ()[m
[32m+[m
[32m+[m
[32m+[m
[32m+[m[32mprivate[engine] sealed trait Stop[m
[32m+[m[32mprivate[engine] case object Yielded extends Stop[m
[32m+[m[32mprivate[engine] case object Completed extends Stop[m
\ No newline at end of file[m
