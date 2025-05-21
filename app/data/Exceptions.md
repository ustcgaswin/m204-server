## Exceptions

**Background**

A software developer will inevitably encounter errors.  Exceptions are a mechanism for handling these errors gracefully.  They allow a program to respond to exceptional situations without crashing.  Exceptions are objects that encapsulate information about an error.  They are typically raised when a program encounters an exceptional condition, such as a file not being found, an invalid input, or a division by zero.  The program can then handle the exception appropriately, such as displaying an error message to the user or logging the error.

**Errors in exception handling**

1.  **The ability to isolate exceptions and the available error classes:**  Knowing the specific error classes available in a programming language is crucial for effective exception handling.  This allows for targeted exception handling.

2.  **The ability to access information in exception objects:**  Exception objects contain details about the error, such as the error message, the location in the code where the error occurred, and the type of error.  This information is essential for debugging and troubleshooting.

**Exception class definitions**

Exception classes are defined by the programming language.  They are used to categorize different types of errors.  Different programming languages may have different exception classes.

**Throwing exceptions**

The `throw` keyword is used to explicitly raise an exception.  This is done when a method or function encounters an error condition that it cannot handle.

**Catching exceptions**

The `try...catch` block is used to handle exceptions.  The `try` block contains the code that might raise an exception.  The `catch` block contains the code that will be executed if an exception is raised within the `try` block.

**Using `try...catch` statements**

This section describes how to use `try...catch` statements to handle exceptions.  It includes examples of how to use different exception handling techniques.

**Multiple classes on a `catch` or `finally` statement**

Multiple `catch` blocks can be used to handle different types of exceptions.  A `finally` block is used to execute code regardless of whether an exception occurred or not.

**Exception attributes**

This section describes the attributes of exception objects.  It includes examples of how to access these attributes.

**Some differences among languages**

This section highlights the differences in exception handling across various programming languages.  It emphasizes the importance of understanding the specific exception handling mechanisms of each language.

**Rethrowing exceptions**

This section describes how to rethrow exceptions.  It explains the use cases for rethrowing exceptions and provides examples.

**On throw and `catch` compatibility**

This section discusses the compatibility between `throw` and `catch` statements.  It explains how different programming languages handle the compatibility of these statements.