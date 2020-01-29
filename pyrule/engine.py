import rule

def __isvalid(ant:tuple,facts:list):
    if ant[0]=='not':
        return not __isvalid(ant[1],facts)
    if ant[0]=='and':
        return __isvalid(ant[1],facts) and __isvalid(ant[2],facts)
    if ant[0]=='or':
        return __isvalid(ant[1],facts) or __isvalid(ant[2],facts)
    for fact in facts:
        if fact==ant:
            return True
    return False

def __isfullfilled(rule:rule.Rule,facts:list):
    ants=rule.antecedent
    for ant in ants: 
        ant=eval(ant)           
        if __isvalid(ant,facts)==False:
            return False
    return True

def step(agenda:list,rules:list,facts:list):
    agenda.clear()
    flag=False
    for rule in rules:
        if rule.executed:
            continue
        if __isfullfilled(rule,facts):
            agenda+=rule.consequent
            rule.executed=True
            flag=True
    return flag
