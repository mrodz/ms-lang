var arr = [[500, "Hello World", ["A", "B", "C", "D", "E"]], 1, 2, 3];

func main() {
  var i = 0;
  var e = arr[i][arr[2]][arr[3]];
  native "LOAD $e";
  native "SYSCALLRM PrintLn";
}
