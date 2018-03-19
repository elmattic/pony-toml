# pony-toml

Pony-based parser implementation for Tom's Obvious, Minimal Language.

Right now values of the following types are not supported:

- Float
- Datetime
- Array of Tables

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
      server = "192.168.1.1"
      ports = [ 8001, 8001, 8002 ]
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
