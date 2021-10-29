package tilelink

trait TLGeneratorExceptions

case class TLCNotInThisBundle(bundleParameters: TLBundleParameter)
    extends Exception(s"""cannot use TLC with parameter: ${bundleParameters.toString}""")
    with TLGeneratorExceptions
