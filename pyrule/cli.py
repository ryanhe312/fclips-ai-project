import cmd

import rule
import engine

class Pyrule(cmd.Cmd):
    """
    A Toy Python Implementation of CLIPS 
    """

    prompt='pyrule>'
    
    # hidden methods
    def default(self,line):
        if line[0]==';':
            return
        print("Invalid Operation: %s"%line)

    def preloop(self):
        self.reset()

    def precmd(self, line):
        line = line.lower()
        if 'load' not in line and 'save' not in line:
            self.lines.append(line)
        return line

    def reset(self):
        rule.Fact.fact_id=0
        init_fact=rule.Fact([None])
        self.lines=[]
        self.facts=[init_fact]
        self.rules=[]
        self.flags=[]
        self.agenda=[]

    def assertone(self,fact):
        self.facts.append(fact)
        if 'watch facts' in self.flags:
            print("==>"+str(fact))

    def retractone(self,fact_id):
        if 'watch facts' in self.flags:
            for fact in self.facts:
                if fact_id==fact:
                    print("<=="+str(fact))
        if fact_id in self.facts:
            self.facts.remove(fact_id)
        else:
            print("Not such a fact id: %d",fact_id)

    def defruleone(self,rule):
        self.rules.append(rule)
        if 'watch rules' in self.flags:
            print("==>"+str(rule))

    # misc
    def do_save(self, arg):
        'Save previous commands to filename:  SAVE filename(.pyrule)'
        try:
            with open((arg if arg else "knowledge")+'.pyrule', 'w') as f:
                for line in self.lines:
                    print(line,file=f)
        except Exception:
            print("Invalid filename: %s"%arg)       

    def do_load(self, arg):
        'load commands from a file:  LOAD filename(.pyrule)'
        with open((arg if arg else "knowledge")+'.pyrule') as f:
            self.cmdqueue.extend(f.read().splitlines())
        
    def do_reset(self,arg):
        'reset all the facts and rules:  RESET'
        self.reset()

    def do_exit(self,arg):
        'exit from the shell:  EXIT'
        self.reset()
        return True

    # facts
    def do_facts(self,arg):
        'list all facts:  FACTS'
        for fact in self.facts:
            print(str(fact))
        print("\nFor a total of %d fact(s)"% len(self.facts))

    def do_assert(self, arg):
        'assert a fact, must in one line, slot must be in python style:  ASSERT (slot1,slot2,)'
        try:
            fact=rule.Fact(eval(arg))
            self.assertone(fact)
        except Exception:
            print("Invalid syntax: %s"%arg,)

    def do_clear(self,arg):
        'clear all facts:  CLEAR'
        for fact_id in range(rule.Fact.fact_id):
            self.retractone(fact_id)

    def do_retract(self,arg):
        'retract one fact:  RETRACT num/ RETRACT *'
        if arg=='*':
            self.do_clear(arg)
        try:
            for fact_id in map(int,arg.split()):
                self.retractone(fact_id)
        except Exception:
            print("Invalid syntax: %s"%arg)

    # watch
    def do_watch(self,arg):
        'watch some variables: WATCH facts/ WATCH rules/ WATCH agenda'
        accepted=['facts','rules']
        if arg in accepted:
            self.flags.append("watch %s"%arg)
        else:
            print("Invalid syntax: %s"%arg)

    # rules
    def do_rules(self,arg):
        'list all rules: RULES'
        for rule in self.rules:
            print(str(rule))
        print("\nFor a total of %d defrule(s)"% len(self.rules))

    def do_defrule(self,arg):
        """
        define a rule,blank space in fact or agenda must be replaced by /,must in a line: 
        DEFRULE name (fac/t1,) (fac/t2,) => agen/da1 agend/a2
        """
        args=arg.split()
        if len(args)<4 or "=>" not in args:
            print("Not enough arguments: %s"%arg)
            return
        ants=[]
        cons=[]
        flag=False
        name=args[0]
        for arg in args[1:]:
            if arg=="=>":
                flag=True
                continue
            if flag==False:
                ants.append(arg.replace("/"," "))
            else:
                cons.append(arg.replace("/"," "))
        rul=rule.Rule(name,ants,cons)
        self.defruleone(rul)

    def do_ppdefrule(self,arg):
        'pretty print a rule: PPDEFRULE name'
        for rule in self.rules:
            if rule.name==arg:
                print("defrule %s"%rule.name)
                for ant in rule.antecedent:
                    print(ant)
                print("=>")
                for con in rule.consequent:
                    print(con)
                

    # input output
    def do_question(self,arg):
        """
        ask a question and make an assertion, answer must be in python style: 
        QUESTION order What do you want?
        """
        args=arg.split(maxsplit=1)
        if len(args)<2:
            print("Not enough arguments: %s"%arg)
            return
        slot=args[0]
        question=args[1]
        print(question)
        answer=input()
        self.do_assert("('%s','%s')"%(slot,answer))

    def do_print(self,arg):
        'print a line: PRINT line'
        print(arg)

    # forward
    def do_agenda(self,arg):
        'print agendas at present: AGENDA'
        for agenda in self.agenda:
            print(agenda)

    def do_step(self,arg):
        'let engine do a step: STEP'
        engine.step(self.agenda,self.rules,self.facts)
        self.cmdqueue.extend(self.agenda)


if __name__ == '__main__':
    Pyrule().cmdloop()