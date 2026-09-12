CodeMirror.defineMode("asm86", function ()
{
  var keywords1 = /^(adc|add|and|bsf|bsr|bswap|bt|btc|btr|bts|cdq|clc|cld|cli|cmc|cmova|cmovae|cmovb|cmovbe|cmovc|cmove|cmovg|cmovge|cmovl|cmovle|cmovna|cmovnae|cmovnb|cmovnbe|cmovnc|cmovne|cmovng|cmovnge|cmovnl|cmovnle|cmovno|cmovns|cmovnz|cmovo|cmovs|cmovz|cmp|cmpxchg|cwd|dec|div|hlt|idiv|imul|in|inc|insb|insd|insw|lea|lidt|lodsb|lodsd|lodsw|loop|loope|loopne|loopnz|loopz|mov|movsb|movsd|movsw|movsx|movzx|mul|neg|nop|not|or|out|outsb|outsd|outsw|pop|popad|popfd|push|pushad|pushfd|rdrand|rep|sal|sar|sbb|seta|setae|setb|setbe|setc|sete|setg|setge|setl|setle|setmp|setna|setnae|setnb|setnbe|setnc|setne|setng|setnge|setnl|setnle|setno|setns|setnz|seto|sets|setz|shl|shr|sidt|stc|std|sti|stosb|stosd|stosw|sub|test|xadd|xchg|xlatb|xor|xrstor|xsave)\b/i;
  var keywords2 = /^(call|int|iret|ret|ja|jae|jb|jbe|jc|je|jg|jge|jl|jle|jmp|jna|jnae|jnb|jnbe|jnc|jne|jng|jnge|jnl|jnle|jno|jns|jnz|jo|js|jz)\b/i;
  var keywords3 = /^(e?[abcd]x|[abcd][lh]|e?(si|di|bp|sp)|eip)\b/i;
  var keywords4 = /^(d?word|byte|ptr)\b/i;
  // https://repo.or.cz/nasm.git/blob/6a4f0b36c803f2028810022f6ad2d9c22d1483e7:/asm/directiv.dat
  var directives = /^(absolute|bits|common|cpu|debug|default|extern|float|global|static|list|section|segment|warning|sectalign|pragma|export|group|import|library|map|module|org|osabi|safeseh|uppercase|prefix|suffix|gprefix|gsuffix|lprefix|lsuffix|limit|subsections_via_symbols|no_dead_strip|maxdump|nodepend|noseclabels)\b/i;
  // https://repo.or.cz/nasm.git/blob/6a4f0b36c803f2028810022f6ad2d9c22d1483e7:/asm/pptok.dat
  var pptok = /^(\\*ctx|\\*def|\\*empty|\\*env|\\*id|\\*idn|\\*idni|\\*macro|\\*num|\\*str|\\*token|%arg|%assign|%clear|%define|%defstr|%deftok|%depend|%elif\\*|%else|%endif|%endm|%endmacro|%endrep|%error|%exitmacro|%exitrep|%fatal|%iassign|%idefine|%idefstr|%ideftok|%if\\*|%imacro|%irmacro|%include|%ixdefine|%line|%local|%macro|%pathsearch|%pop|%pragma|%push|%rep|%repl|%rmacro|%rotate|%stacksize|%strcat|%strlen|%substr|%undef|%unimacro|%unmacro|%use|%warning|%xdefine)\b/i;
  var numbers = /^(0x[0-9a-f]+|0b[01]+|[0-9]+|[0-9][0-9a-f]+h|[0-1]+b)\b/i;
  return {
    startState: function () {
      return { context: 0 };
    },
    token: function (stream, state) {
      //if (!stream.column())
      //	state.context = 0;
      if (stream.eatSpace())
	return null;
      var w;
      if (stream.eatWhile(/\w/)) {
	w = stream.current();
	if (keywords1.test(w)) {
	  //state.context = 1;
	  return "keyword";
	} else if (keywords2.test(w)) {
	  //state.context = 2;
	  return "keyword-2";
	} else if (keywords3.test(w)) {
	  //state.context = 3;
	  return "keyword-3";
	} else if (keywords4.test(w)) {
	  return "operator";
	} else if (numbers.test(w)) {
	  return "number";
        } else if (directives.test(w)) {
          return "directive";
        } else if (pptok.test(w)) {
          return "pptok";
        } else if (stream.eat(":")) {
          return "label";
	} else {
	  return null;
	}
      } else if (stream.eat(";")) {
	stream.skipToEnd();
	return "comment";
      } else if (stream.eat(",") || stream.eat(".") || stream.eat(":") || stream.eat("[") || stream.eat("]") || stream.eat("+") || stream.eat("-") || stream.eat("*")) {
	return "operator";
      } else {
	stream.next();
      }
      return null;
    }
  };
});
CodeMirror.defineMIME("text/x-asm86", "asm86");
