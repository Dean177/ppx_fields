module Form = {
  [@deriving fields]
  type t = {
    email: string,
    age: int,
  };
};

open Form;

let user = {email: "user@example.com", age: 3};

Js.log(Fields.get(user, Age));

Js.log(Fields.get(user, Email));

Js.log(
  Fields.fold(user, ~initial=[], ~f=(strings, field) =>
    switch (field) {
    // Unfortunately type annotations are needed sometimes for these 😞
    // https://stackoverflow.com/questions/57452349/how-do-i-extract-useful-information-from-the-payload-of-a-gadt-existential-typ
    | Fields.Field(Fields.Email, (email: string)) => [email, ...strings]
    | Fields.Field(Fields.Age, age) => [string_of_int(age), ...strings]
    }
  ),
);