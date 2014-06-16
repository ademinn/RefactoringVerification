class Node {
    Node next;
    int value;

    Node(int i) {
        value = i;
    }
}

class List {
    Node head;

    void addNode(int value) {
        Node n = new Node(value);
        n.next = head;
        head = n;
    }

    void print() {
        Node cur = head;
        ConsoleWriter cw = new ConsoleWriter();
        while (cur != null) {
            cw.writeInt(cur.value);
            cur = cur.next;
        }
    }
}

class Main {
    Main () {
        List l = new List();
        for (int i = 0; i < 10; i++) {
            l.addNode(i);
        }
        l.print();
    }
}
