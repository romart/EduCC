// C99 6.8.6.1p1: a jump may not enter the scope of an identifier with a
// variably modified type.
//
// This is a frontend hole step 19 had to close rather than a rule it wanted to
// enforce. A variable-length array is carved out of the stack where its
// declaration runs, and the block gives that storage back by restoring the
// stack pointer to a mark taken at the declaration; a jump that lands below
// the declaration reaches the array without having allocated it and reaches
// the restore without having taken the mark. Before the check, such a program
// parsed cleanly and then either aborted the compiler in DCE (the 'case'
// spellings below, whose scope is entered from a block the switch's own head
// never reaches) or silently produced code that moved the stack pointer to
// whatever happened to be in the saved slot.
//
// gcc rejects every one of these, with these two messages and at these
// coordinates.
//
// What is deliberately *not* an error is a label above the declaration:
// 'beforeTheArray' below is outside the array's scope, so jumping to it is
// legal C, and the mark being taken at the declaration rather than at the head
// of the block is what makes that work here too.

extern int n;

// -------- goto, forwards into the scope --------

void gotoForward(void) {
    if (n) goto inside;
    {
        int v[n];
    inside:
        v[0] = 1;
    }
}

// -------- goto, from a sibling block into the scope --------

void gotoSideways(void) {
    {
        int a[n];
        a[0] = 0;
        goto other;
    }
    {
        int b[n];
    other:
        b[0] = 1;
    }
}

// -------- goto, from outside two nested scopes --------

// Reported once, at the jump, however many scopes it would enter.
void gotoNested(void) {
    goto deep;
    {
        int outer[n];
        {
            int inner[n];
        deep:
            inner[0] = outer[0];
        }
    }
}

// -------- a case label inside the scope --------

void switchCase(void) {
    switch (n) {
        int v[n];
    case 4:
        v[0] = 1;
        break;
    }
}

// -------- a default label inside the scope --------

void switchDefault(void) {
    switch (n) {
        int v[n];
    default:
        v[0] = 1;
        break;
    }
}

// -------- a case label inside a nested block that allocates --------

// The switch head jumps straight to it, past the declaration, exactly as the
// flat spelling above does.
void switchNestedCase(void) {
    switch (n) {
    case 1: {
        int v[n];
    case 2:
        v[0] = 1;
        break;
    }
    }
}

// -------- not the mistake: a label above the declaration --------

// 'beforeTheArray' is not in the array's scope, so this is legal C and gcc
// accepts it. Nothing below is reported.
void jumpToLabelAboveDeclaration(void) {
    if (n) goto beforeTheArray;
    {
    beforeTheArray:
        ;
        int v[n];
        v[0] = 1;
    }
}

// -------- not the mistake: a case label above the declaration --------

void caseAboveDeclaration(void) {
    switch (n) {
    case 1:
        ;
        int v[n];
        v[0] = 1;
        break;
    }
}

// -------- not the mistake: jumping out of a scope --------

void gotoOutOfScope(void) {
    {
        int v[n];
        v[0] = 1;
        goto done;
    }
done:
    ;
}
