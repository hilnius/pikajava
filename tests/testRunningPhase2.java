// Tout est typé correctement normalement, mais tout ne s'execute pas.
// Ci-après quelques lignes de code permettant de voir ce qui s'execute.
class NewClass
{
	protected int newAttribute = 1;
	protected int newSecondAttribute;

	NewClass()
	{
		this.newAttribute = 2;
	}

	public int getNewAttribute()
	{
		return this.newAttribute;
	}

	public int addToSecondAttribute(int valueToAdd)
	{
		int a = 2;
		// Addition
		this.newSecondAttribute += valueToAdd;
		return this.newSecondAttribute;
	}

	public void changeAttributeFirstValue(int newValue, NewClass objectToModify)
	{
		objectToModify.newAttribute = newValue;
	}

	public static void main(int a)
	{
		System sys = new System();
		sys.println("DEBUT DU MAIN");
		int c = 2;
		NewClass alpha = new NewClass();
		SecondNewClass beta = new SecondNewClass();

		// Accès aux attributs
		sys.println("Accès aux attributs");
		sys.println(alpha.newSecondAttribute);

		// Modification des attributs dans les méthodes
		sys.println("Modification des attributs dans les méthodes");
		alpha.addToSecondAttribute(3);
		sys.println(alpha.newSecondAttribute);

		//Modification des attributs dans les méthodes
		sys.println("Modification des attributs dans les méthodes");
		beta.addToSecondAttribute(3);
		sys.println(beta.newSecondAttribute);

		// Incrémentation après execution de la boucle
		sys.println("Incrémentation après execution de la boucle");
		for (int i = 0; i<3; i++)
		{
			sys.println(i);
		}

		// Incrémentation avant execution de la boucle
		sys.println("Incrémentation avant execution de la boucle");
		for (int j = 0; j<3; ++j)
		{
			sys.println(j);
		}

		// While
		sys.println("boucle while");
		int w = 1;
		while (w > 0)
		{
			w--;
			sys.println(w);
		}

		// Les boucles if
		if (w < 0)
		{
			sys.println("La dernière valeur affichée est négative");
		}
		else if (w == 0)
		{
			sys.println("La dernière valeur affichée est nulle");
		}
		else
		{
			sys.println("La dernière valeur affichée est positive");
		}

		// Les condop
		sys.println("Les condop");
		w = (w==0)? 8 : 10;
		sys.println(w);

		// Les references
		sys.println("Les references");
		sys.println(beta.newAttribute);
		alpha.changeAttributeFirstValue(11, beta);
		sys.println(beta.newAttribute);

		// instanceof
		sys.println("instanceof");
		if (beta instanceof SecondNewClass)
		{
			sys.println("beta est une instance de SecondNewClass");
		}
		if (alpha instanceof SecondNewClass)
		{
			sys.println("alpha est une instance de SecondNewClass");
		}
		else
		{
			sys.println("alpha n'est pas une instance de SecondNewClass");
		}
		
		//booleens
		sys.println("les booléens True et False");
		if (true)
		{
			sys.println("True!");
		}

		if (!false)
		{
			sys.println("False!");
		}

		sys.println("FIN DU MAIN");
	} 
}

class SecondNewClass extends NewClass
{
	SecondNewClass() {}

	public int addToSecondAttribute(int valueToAdd)
	{
		int a = 2;
		// Addition avec a en plus
		this.newSecondAttribute += valueToAdd + a;
		return this.newSecondAttribute;
	}
}

class System
{
	System() {}
	public void println(){}
}