var greeting = 0;

func main() {
  cat();
}

func cat() {
  greeting += 1;

  native "LOAD $greeting";
  native "SYSCALLRM PrintLn";
}