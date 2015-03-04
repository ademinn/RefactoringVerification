class A extends Object {
    A() {
        super();
    }
}

class Pair extends Object {
    A first;
    A second;

    Pair(A first, A second) {
        super();
        this.first = first;
        this.second = second;
    }

    Pair copy() {
        return new Pair(this.first, this.second);
    }
}

class PairFactory extends Object {
    PairFactory () {
        super();
    }

    A createA() {
        return new A();
    }

    Pair createPair() {
        return new Pair(this.createA(), this.createA());
    }
}
