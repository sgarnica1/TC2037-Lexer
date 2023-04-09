using System;

// This is a line comment

class Program
{
    static void Main(string[] args)
    {
        int[] numbers = {1, 2, 3, 4, 5};
        for (int i = 0; i < numbers.Length; i++)
        {
            if (numbers[i] % 2 == 0)
            {
                Console.WriteLine(numbers[i] + " is even");
            }
            else
            {
                Console.WriteLine(numbers[i] + " is odd");
            }
        }

        Console.WriteLine();

        string message = "Hello, world!";
        for (int i = 0; i < message.Length; i++)
        {
            Console.Write(message[i] + " ");
        }
        Console.WriteLine();

        MyClass myObject = new MyClass();
        myObject.MyMethod();
    }
}

class MyClass
{
    public void MyMethod()
    {
        Console.WriteLine("MyClass.MyMethod() was called");
    }
}