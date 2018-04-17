package kdkvsk.hackuo.script

import javax.script.ScriptEngineManager

object Script {
  def main(args: Array[String]): Unit = {
    val engine = new ScriptEngineManager().getEngineByName("JavaScript")
    val test = new Object {
      def test(): Unit = {
        System.out.println("test")
      }
    }
    engine.put("test", test)

    engine.eval("""test.test()""")
  }

}
