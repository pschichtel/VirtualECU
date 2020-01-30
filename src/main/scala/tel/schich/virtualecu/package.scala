package tel.schich

package object virtualecu {

  type TimeSeriesCompiler = String => TimeSeriesScript
  type TimeSeriesScript = Double => Array[Byte]

}
