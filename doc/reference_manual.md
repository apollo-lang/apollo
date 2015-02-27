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

	func (x: Int) -> String {
	  case x < 10 {
	    'hi'
	  } case x > 10 {
	    'ho'
	  } otherwise {
	    'hey'
	  }
	}

	func (x: Int) -> String
	    case x < 10
		'hi'
	    case x > 10
	       'ho'
	    otherwise
	       'hey'
	    end
	end

	def fff(x) if x > 10 then 'hi' else 'ho' end end

	def fff(x) { if x > 10 { 'hi' } else { 'ho' } }

