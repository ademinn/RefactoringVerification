class A {
    A() {}
}

class Pair {
    A first;
    A second;

    Pair(A first, A second) {
        this.first = first;
        this.second = second;
    }

    Pair copy() {
        return new Pair(this.first, this.second);
    }
}

class PairFactory {
    PairFactory () {}

    A createA() {
        return new A();
    }

    Pair createPair() {
        return new Pair(this.createA(), this.createA()).copy();
    }
}
