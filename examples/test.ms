func main() {
  if false {
  } else if false {
    print(2);
  } else {
    print("a");
    if false {
      print("b");
    } else if true {
      print("c");
    }
    print("d");
  }
}

func print(a) {
  native "LOAD $a";
  native "SYSCALLRM PrintLn";
}