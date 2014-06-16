class Main {
    Main () {
        ConsoleWriter w = new ConsoleWriter();
        for (int i = 1; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                if (j == i) {
                    break;
                }
                w.writeInt(j);
            }
        }
    }
}
