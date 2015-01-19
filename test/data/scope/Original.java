class Test extends Object {
    Test() {
        super();
    }
}

class Pair extends Object {
    Object a;
    Object b;

    Pair(Object a, Object b) {
        super();
        this.a = a;
        this.b = b;
    }
}

class PairFactory extends Object {
    PairFactory() {
        super();
    }

    Pair createPair() {
        return new Pair(new Object(), new Object());
    }
}

class TestPairFactory extends PairFactory {
    TestPairFactory() {
        super();
    }

    Pair createTestPair() {
        return new Pair(new Test(), new Test());
    }
}
