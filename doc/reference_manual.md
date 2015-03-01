# Apollo Language Reference Manual

## Syntax

- No semicolons
- Braces (?)

## Data Types

### Primitives

- int
- float
- bool
- char
- list

## Functions

Comparison of braces vs `end`:

	func greet(x: Int) -> String {
	  case x < 10 {
	    'hi'
	  } case x > 10 {
	    'ho'
	  } otherwise {
	    'hey'
	  }
	}

	def greet(x: Int) -> String
	    case x < 10
	        'hi'
	    case x > 10
	       'ho'
	    otherwise
	       'hey'
	    end
	end

	func greet(x) { if x > 10 { 'hi' } else { 'ho' } }

	def greet(x) if x > 10 then 'hi' else 'ho' end end
