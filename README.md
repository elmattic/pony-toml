# pony-toml

Pony-based parser implementation for Tom's Obvious, Minimal Language.

Right now values of the following types are not supported:

- String types that are not basic
- Float
- Datetime
- Array
- Inline Table
- Array of Tables

Quoted and dotted keys are also not supported.
That being said the goal is to be compliant with the v0.4.0 version of TOML and
stay up to date as the specification evolves.

## Usage

Here is a simple code sample:

```pony
actor Main
  new create(env: Env) =>
    let parser = Parser.from_string(
      """
      # This is a TOML document.

      title = "TOML Example"

      [owner]
      name = "Tom Preston-Werner"

      [database]
      connection_max = 5000
      enabled = true
      """)
    match parser.parse()
    | let doc: TOMLTable =>
      env.out.print(doc.string()) // JSON like output
      try
        match doc.map("database")?
        | let database: TOMLTable =>
          env.out.print(database.map("connection_max")?.string())
        end
      end
    | let err: Error => env.out.print(err.message())
    end
```
