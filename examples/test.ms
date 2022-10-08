var a = 5;
var b = 10;

func main() {
  var c = 3;
  var x = ((a + b) % c);

  native "LOAD $x";
  native "SYSCALLRM Print";

  
}