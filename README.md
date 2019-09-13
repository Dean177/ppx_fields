# ppx_fields

## Installation

Install the last stable version

```
yarn add ppx_fields
```

## Usage

Annotate a record with `[%fields]`

```reason
module FormState = {
  [@deriving fields]
  type t = {
    email: string,
    age: int,
  };
}
```

And you get the following generated for you:

```reason
module FormState = {
  type t = {
    email: string,
    age: int,
  };

  module Fields = {
    type fieldName(_) =
      | Email: fieldName(string)
      | Age: fieldName(int);

    type field =
      | Field(fieldName('a), 'a): field;

    let get: type value. (t, fieldName(value)) => value;

    let set: type value. (t, fieldName(value), value) => t;

    let compare: (fieldName('a), fieldName('b)) => int;

    let fold: (t, ~initial: 'result, ~f: ('result, field) => 'result) => 'result;
  };
};
```

# Developing

Install [esy](esy.sh)

```sh
yarn global add esy

esy
```

See esy.json for the available scripts.
