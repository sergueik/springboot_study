make that comparison as an analogy, but with some important caveats. COBOL REDEFINES is not really an early form of object-oriented inheritance. It is closer to an early form of type punning / memory overlay / union types. However, the problem it solves is one of the same problems that later OO techniques addressed: representing multiple related views of the same underlying data structure.
For example, COBOL:
01 SCREEN-DATA.
   05 FIELD-A PIC X(10).
   05 FIELD-B PIC 9(5).

01 CUSTOMER-SCREEN REDEFINES SCREEN-DATA.
   05 CUSTOMER-NAME PIC X(10).
   05 CUSTOMER-ID   PIC 9(5).
means:
"These two descriptions occupy the same bytes. Interpret this storage area differently depending on context."
There is no notion of:
    • behavior, 
    • methods, 
    • dispatch, 
    • encapsulation, 
    • substitutability, 
    • "is-a" relationship. 
It is literally:
same memory
  |
  +-- interpretation A
  |
  +-- interpretation B
The OO inheritance model is more like:
             Screen
               |
       +-------+-------+
       |               |
   CustomerScreen   OrderScreen

       render()       render()
       validate()     validate()
where the objects share a contract and behavior can vary.

That said, your intuition points at a real historical connection. Large COBOL systems often had structures like:
01 TRANSACTION.
   05 COMMON-HEADER.
   05 PAYLOAD.

01 PAYMENT-TRANSACTION REDEFINES TRANSACTION.
   05 COMMON-HEADER.
   05 PAYMENT-FIELDS.

01 REFUND-TRANSACTION REDEFINES TRANSACTION.
   05 COMMON-HEADER.
   05 REFUND-FIELDS.
Conceptually, someone later designing an OO system might naturally think:
Transaction
   |
   +-- PaymentTransaction
   |
   +-- RefundTransaction
The data modeling instinct is similar. The difference is that COBOL says:
"The programmer promises that this area can be interpreted in several ways."
OO says:
"The object itself carries its type and behavior, and clients interact through a common interface."

Your terminal-screen example is actually a very good one. A 1970s/1980s CICS programmer might have:
SCREEN-BASE
   common fields

SCREEN-A REDEFINES SCREEN-BASE
   extra fields

SCREEN-B REDEFINES SCREEN-BASE
   different fields
The OO designer might later create:
abstract class Screen {
    draw();
    validate();
}

class ScreenA extends Screen {
    draw();
    validate();
}

class ScreenB extends Screen {
    draw();
    validate();
}
The motivation is related: avoid duplicating common structure.
But the implementation philosophy is almost opposite:
COBOL REDEFINES	OO inheritance
same bytes, different interpretation	different objects, shared contract
static choice by programmer	dynamic behavior selection
data-centric	behavior-centric
memory layout first	abstraction first
unsafe if misunderstood	designed for substitution
So a historically fair statement would be:
COBOL REDEFINES can be seen as an early data-structure technique that addressed some of the same modeling pressures that later led to inheritance and polymorphism, but it is not inheritance itself.
A more provocative (and perhaps closer to your intuition) version:
REDEFINES was one of the pre-OO ways programmers dealt with "one thing having multiple forms"; OO later moved that idea from the memory layout level into the type-and-behavior level.
That transition—from "interpret these bytes differently" to "this object behaves differently"—is essentially one of the big conceptual jumps from procedural to object-oriented design.

