func main() {
  var my_array = [1, ["a", "b"]];
  var elem = my_array[1][1];

  native "LOAD $elem";
  native "SYSCALLRM PrintLn";
}