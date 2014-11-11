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

    Pair createPair() {
        return new Pair(new A(), new A());
    }
}
