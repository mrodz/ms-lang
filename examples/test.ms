var greeting = 3;

func main() {
  cat();
}

func cat() {
  greeting *= 3;

  native "LOAD $greeting";
  native "SYSCALLRM PrintLn";
}