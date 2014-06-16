class Main {
    Main () {
        ConsoleWriter w = new ConsoleWriter();
        for (int i = 2; i < 4; i++) {
            for (int j = 0; j < 10; j++) {
                if (j % i == 0) {
                    continue;
                }
                w.writeInt(j);
            }
        }
    }
}
