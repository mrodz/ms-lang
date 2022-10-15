var arr = [[500, "Hello World"], 2, 3];


func main() {
  var i = 0;
  var j = 1;

  var e = arr[i][j];
  native "LOAD $e";
  native "SYSCALLRM PrintLn";
}
