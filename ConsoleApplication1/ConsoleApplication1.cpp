using namespace System;
using namespace System::IO;
using namespace System::Collections::Generic;
using namespace EmptyBox::Mathematics::Expressions;

int main()
{
	ScalarFunction^ x = ScalarFunction::NewVariable("x");
	ScalarFunction^ f = x * x * x * 2;
	Console::WriteLine(f);
	Console::WriteLine(f->Calculate());
    return 0;
}

