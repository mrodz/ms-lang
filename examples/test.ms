var a = "Hello World!";
var pi = 3.141592;

func main() {
  var my_array = [1, 2, a, pi, ["woo", "back", [["baby"]]]];

  native "LOAD $my_array";
  native "SYSCALLRM PrintLn";
}