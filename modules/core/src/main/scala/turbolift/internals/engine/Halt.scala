package turbolift.internals.engine


private[turbolift] enum Halt:
  case ContinueNoTick
  case Continue
  case Reset
  case Become
  case Yield
  case Retire
