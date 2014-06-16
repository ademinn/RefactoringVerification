class Main {
    Main() {
        int i = 2;
        if (true && (i > 1 || i < -1)) {
            new ConsoleWriter().writeInt(1);
        } else {
            new ConsoleWriter().writeInt(2);
        }
    }
}
