package turbolift.interpreter


private[interpreter] trait Features_opaque:
  extension (thiz: Features)
    inline def |(that: Features): Features = Features.wrap(thiz.unwrap | that.unwrap)
    inline def mask: Features = Features.wrap(thiz.unwrap & Features.Mask.unwrap)
