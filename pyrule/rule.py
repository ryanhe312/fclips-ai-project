from pprint import pformat

class Fact:
    """
    CLIPS Fact
    """
    fact_id = 0

    def __init__(self,slots:list):
        'Initial steps for a Fact'
        self.id = None
        self.slots = tuple(slots)
        self.__calculate_id__()

    def __calculate_id__(self):
        'Give the fact an ID'
        self.id = self.fact_id
        self.fact_id += 1

    def __eq__(self,other):
        'can be called by retracting num'
        if type(other) is int:
            return self.id==other
        if type(other) is tuple:
            for i in range(min(len(self.slots),len(other))):
                if self.slots[i]=='?' or other[i]=='?':
                    continue
                if self.slots[i]!=other[i]:
                    return False
            return True
        return False
                

    def __str__(self):
        'Return slots for print'
        return "f-"+str(self.id)+' '+pformat(self.slots)

class Rule:
    """
    CLIPS Rule
    """
    def __init__(self,name:str,ants:list, cons:list):
        """
        Initial steps for a rule
        """
        self.name = name
        self.antecedent = ants
        self.consequent = cons
        self.executed=False

    def __str__(self):
        """
        Return name for print
        """
        return self.name