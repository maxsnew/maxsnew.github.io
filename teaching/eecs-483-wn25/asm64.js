CodeMirror.defineMode("asm64", function ()
{
  // https://www.felixcloutier.com/x86/
  var commands = /^(adc|add[bwlq]?|and[bwlq]?|bsf|bsr|bswap|bt|btc|btr|bts|cdq|clc|cld|cli|cltd|cmc|cmov(a|ae|b|be|c|e|g|ge|l|le|o|p|pe|po|s|z)|cmovn(a|ae|b|be|c|e|g|ge|l|le|o|p|s|z)|cmp[bwlq]?|cmpxchg|cqto|cwd|dec[bwlq]?|divl|divq|hlt|idivl|idivq|imul[bwlq]?|in|inc[bwlq]?|insb|insd|insw|lea[bwlq]?|lidt|lodsb|lodsd|lodsw|loop|loope|loopne|loopnz|loopz|mov[bwlq]?|movs(bw|bl|bq|wl|wq|lq|s|d)|movz(bw|bl|bq|wl|wq)|movabsq|mul|neg[bwlq]?|nop|not[bwlq]?|or[bwlq]?|out|outsb|outsd|outsw|popq|pushq|rdrand|rep|sal[bwlq]?|sar[bwlq]?|sbb|set(a|ae|b|be||c|e|g|ge|l|le|o|p|pe|po|s|z)|setn(a|ae|b|be|c|e|g|ge|l|le|o|p|s|z)|shl[bwlq]?|shr[bwlq]?|sidt|stc|std|sti|stosb|stosd|stosw|sub[bwlq]?|test[bwlq]?|xadd|xchg|xlatb|xor[bwlq]?|xrstor|xsave)\b/i;
  var controls = /^(call|int|iret|ret|ja|jae|jb|jbe|jc|je|jg|jge|jl|jle|jmp|jna|jnae|jnb|jnbe|jnc|jne|jng|jnge|jnl|jnle|jno|jns|jnz|jo|js|jz)\b/i;
  var registers = /^([re]?[abcd]x|[abcd][hl]|[re]?(si|di|bp|sp)|(si|di|bp|sp)l|r(8|9|10|11|12|13|14|15)[dwb]?|rip)|xmm(0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15)\b/i;
  var sizes = /^([qd]word|byte|ptr)\b/i;
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
	if (commands.test(w)) {
	  //state.context = 1;
	  return "keyword";
	} else if (controls.test(w)) {
	  //state.context = 2;
	  return "keyword-2";
	} else if (registers.test(w)) {
	  //state.context = 3;
	  return "keyword-3";
	} else if (sizes.test(w)) {
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
CodeMirror.defineMIME("text/x-asm64", "asm64");
