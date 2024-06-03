# Introduction

This repository contains a working web service application for Paidride. Your task is to refactor this application using the functional programming principles discussed during the course. Furthermore, you will implement additional use cases.

# Model

The application currently models employees and hours

## Employee

An employee works for a department. 

1. has a *unique *name that is not empty,
2. has an associated department id.
3. has many dates with registered hours

## Registered hours

Every row

1. has a date on which the hours were registered
2. has the name of the employee
3. and the amount of hours the employee worked

## Department

A department

1. has a unique id that is not empty and consists of four capital letters followed by two digits
2. has a name that is not empty, and consists of only letters and spaces (two or more consecutive spaces are not allowed)
3. optionally has an associated department of which this department is a part

# Code discussion

## `Program.md`

1. The application starts in the `main` function.
2. The function `configureApp` registers the routes defined in `Web.fs`.
3. The function `configureServices` registers the data store and JSON-library Thoth that are used by the HttpHandlers in `Web.fs`.

## `Database.md`

Defines a simple in-memory database that stores objects associated with a primary key. The objects are of type `'T`. Keys are of type `'Key`.

```fsharp
type InMemoryDatabase<'Key, 'T when 'Key: comparison>
```

## `Store.fs`

Defines separate in-memory databases to store employees, hours and departments. 

If you study the relations defined in `Store.fs` you will notice that each table is represented my a member of the `Store`. The type of each table is parametric. The first type parameter represents the type of the primary key, which may be compound. The second represents the type of data stored in the table.

For instance, the `hours` member represents the hours registered by an employee on a particular day. The primary key here is a tuple type: `string * DateTime`, a combination of the employee's name and the date for which hours are registered. The data itself is a triple, consisting of the employee's name, the date and the amount of hours registered.

```fsharp
member val hours: InMemoryDatabase<string * DateTime, string * DateTime * int> =
```

### Tables
`employees` stores information about employees. Each row contains

1. the employee's name (a string and the primary key)
2. the id of the department (a string of four letters followed by two digits)

`hours` stores the hours registered. Each row contains

1. the employee's name as a string (part of the primary key together with the next column)
2. the date for which the hours are registered as a `DateTime`-value
3. the number of hours the employee worked on that date.

`departments` stores information about the departments. Each row contains

1. The department's id (a string and its primary key)
2. The department's name
3. A nullable (optional) id of the larger department to which this department may belong.

## `Model.fs`

Defines structures for Employees, Hours and Departments. It also contains JSON serialisation and deserialisation functionality.

**NOTE:** You do not need to serialize or deserialize `Department` values. This is more involved as departments are defined recursively. In situations where you feel you must return department information, their identifiers alone will suffice.

## `Web.fs`

Contains the http handlers
