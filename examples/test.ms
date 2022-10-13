func main() {
  print_first_element([1, true && false, 3 + 2 * 10]);
}

func print_first_element(array) {
  var middle = array[1];
  native "LOAD $array, $middle";
  native "SYSCALLRM PrintLn";
}