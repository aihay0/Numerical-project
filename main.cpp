#include <bits/stdc++.h>
using namespace std;
#define f first
#define s second
const long double E=exp(1);
const long double PI=acos(-1);
string tokens[]={"(",")","*","/","+","-","^","sin","cos","tan","ln","sqrt","pi","e"};
enum tokentype {Bracket,Operation,Function,Constant,Variable,Invalid};
struct token{
	tokentype type;
	string value;
	bool eval=0;
	long double res=0;
	long double coef=1;
	long double pw=1;
};
bool error;
long double val(token t){
	if(t.type!=Constant)
		return 0;
	if(t.eval)
		return t.res;
	else if(t.value=="e")
		return E;
	else if(t.value=="pi")
		return PI;
	long double ans=t.coef*pow(stold(t.value),t.pw);
	if(isnan(ans))
		return 0;
	return ans;
}
struct Expression{
	vector<token> v;
};
struct Var{
	string name;
	long double value;
	bool operator<(const Var &y){
		return name<y.name;
	}
};
bool istoken(string s){
	for(auto it:tokens){
		if(s==it)
			return 1;
	}
	return 0;
}
tokentype gettype(string s){
	if(isupper(s[0]))
		return Variable;
	for(int i=0;i<14;i++){
		if(s!=tokens[i])
			continue;
		if(i<2)
			return Bracket;
		if(i<7)
			return Operation;
		if(i<12)
			return Function;
		return Constant;
	}
	return Invalid;
}
void combine(Expression &e1,Expression &e2){
	for(auto it:e2.v)
		e1.v.push_back(it);
	return;
}
struct Func{
	string expression;
	vector<token> tokenvec;
	Expression parsed;
	set<string> varnames;
	bool built=0;
	Func(){
	}
	Func(string s){
		expression=s;
		build();
	}
	Func(Expression e){
		parsed=e;
		for(auto it:e.v){
			if(it.type==Variable)
				varnames.insert(it.value);
		}
		built=1;
	}
	void print(){
		for(auto it:parsed.v){
			if(it.type==Constant)
	    		cout<<val(it)<<" ";
	    	else{
	    		if(it.coef!=1)
	    			cout<<(double)it.coef;
	    		cout<<it.value;
	    		if(it.pw!=1)
	    			cout<<"^"<<(double)it.pw;
	    		cout<<" ";
	    	}
		}
		cout<<"\n";
		return;
	}
	Expression parse(int l,int r){
		if(l==r)
			return {{tokenvec[l]}};
		Expression result;
		vector<Expression> cur;
		for(int i=l;i<=r;i++){
			if(tokenvec[i].type==Bracket){
				int cnt=1,idx=i+1;
				while(idx<=r){
					if(tokenvec[idx].type!=Bracket){
						idx++;
						continue;
					}
					if(tokenvec[idx].value==")")
						cnt--;
					else
						cnt++;
					if(cnt==0)
						break;
					idx++;
				}
				if(cnt!=0)
					return {};
				cur.push_back(parse(i+1,idx-1));
				i=idx;
				continue;
			}
			if(cur.size()&&(cur.back().v.size()!=1||(cur.back().v[0].type!=Operation&&cur.back().v[0].type!=Function))&&tokenvec[i].type!=Operation){
				token t={Operation,"*"};
				Expression e;
				e.v.push_back(t);
				cur.push_back(e);
			}
			if(cur.empty()&&tokenvec[i].value=="-"){
				token t={Constant,"0"};
				Expression e;
				e.v.push_back(t);
				cur.push_back(e);
			}
			Expression e;
			e.v.push_back(tokenvec[i]);
			cur.push_back(e);
		}
		//Functions
		{
			vector<Expression> nw;
			for(int i=0;i<cur.size();i++){
				if(cur[i].v.size()>1||cur[i].v.back().type!=Function){
					nw.push_back(cur[i]);
					continue;
				}
				if(i+1==cur.size())
					return {};
				Expression e=cur[i+1];
				Expression e2;
				token t={Function,cur[i].v.back().value};
				e2.v.push_back(t);
				combine(e,e2);
				nw.push_back(e);
				i++;
			}
			cur=nw;
		}
		//Exponents
		{
			vector<Expression> nw;
			for(int i=0;i<cur.size();i++){
				if(cur[i].v.size()>1||cur[i].v[0].value!="^"){
					nw.push_back(cur[i]);
					continue;
				}
				if(nw.empty()||i+1==cur.size())
					return {};
				Expression e=nw.back();
				nw.pop_back();
				combine(e,cur[i+1]);
				combine(e,cur[i]);
				nw.push_back(e);
				i++;
			}
			cur=nw;
		}
		//Multiplication and Division
		{
			vector<Expression> nw;
			for(int i=0;i<cur.size();i++){
				if(cur[i].v.size()>1||(cur[i].v[0].value!="*"&&cur[i].v[0].value!="/")){
					nw.push_back(cur[i]);
					continue;
				}
				if(nw.empty()||i+1==cur.size())
					return {};
				Expression e=nw.back();
				nw.pop_back();
				combine(e,cur[i+1]);
				combine(e,cur[i]);
				nw.push_back(e);
				i++;
			}
			cur=nw;
		}
		//Addition and Subtraction
		{
			vector<Expression> nw;
			for(int i=0;i<cur.size();i++){
				if(cur[i].v.size()>1||(cur[i].v[0].value!="+"&&cur[i].v[0].value!="-")){
					nw.push_back(cur[i]);
					continue;
				}
				if(nw.empty()||i+1==cur.size())
					return {};
				Expression e=nw.back();
				nw.pop_back();
				combine(e,cur[i+1]);
				combine(e,cur[i]);
				nw.push_back(e);
				i++;
			}
			cur=nw;
		}
		if(cur.size()!=1)
			return {};
		return cur[0];
	}
	bool parse(){
		if(tokenvec.size()==0)
			return 0;
		parsed=parse(0,tokenvec.size()-1);
		return !parsed.v.empty();
	}
	bool readExpression(){
		tokenvec.clear();
		parsed.v.clear();
		varnames.clear();
		int n=expression.size();
		for(int i=0;i<n;i++){
			if(expression[i]==' '||expression[i]=='\n'||expression[i]=='\t')continue;
			if(isdigit(expression[i])){
				string s="";
				while(i<n&&(isdigit(expression[i])||expression[i]=='.'||expression[i]=='e'))
					s+=expression[i++];
				tokenvec.push_back({Constant,s});
				i--;
				continue;
			}
			if(isupper(expression[i])){
				string s="";
				while(i<n&&(isalpha(expression[i])||isdigit(expression[i])))
					s+=expression[i++];
				tokenvec.push_back({Variable,s});
				varnames.insert(s);
				i--;
				continue;
			}
			string s="";
			while(i<n&&!istoken(s))
				s+=expression[i++];
			i--;
			if(!istoken(s))
				return 0;
			tokenvec.push_back({gettype(s),s});
		}
		return 1;
	}
	bool build(){
		built=0;
		if(!readExpression()) return 0;
		built=parse();
		return built;
	}
	long double Evaluate(map<string,long double> mp){
		stack<long double> st;
		for(auto it:parsed.v){
			if(it.type==Constant)
				st.push(val(it));
			else if(it.type==Variable)
				st.push(it.coef*pow(mp[it.value],it.pw));
			else if(it.type==Function){
				long double x=st.top();
				st.pop();
				if(it.value=="sin")
					st.push(sin(x));
				else if(it.value=="cos")
					st.push(cos(x));
				else if(it.value=="tan")
					st.push(tan(x+1e-30));
				else if(it.value=="ln"){
					if(x<=0)
						st.push(0),error=1;
					else
						st.push(log(x));
				}
				else if(it.value=="sqrt"){
					if(x<0)
						st.push(0),error=1;
					else
						st.push(sqrt(x));
				}
			}
			else if(it.type==Operation){
				long double x,y=st.top();
				st.pop();
				x=st.top();
				st.pop();
				if(it.value=="+")
					st.push(x+y);
				else if(it.value=="-")
					st.push(x-y);
				else if(it.value=="*")
					st.push(x*y);
				else if(it.value=="/"){
					if(y==0)
						st.push(0),error=1;
					else
						st.push(x/y);
				}
				else if(it.value=="^")
					st.push(pow(x,y));
			}
			if(isnan(st.top())||isinf(st.top())){
				st.pop();
				st.push(0);
				error=1;
			}
		}
		return st.top();
	}
	long double Evaluate(vector<Var> vars){
		map<string,long double> mp;
		for(auto it:vars)
			mp[it.name]=it.value;
		return Evaluate(mp);
	}
	long double operator()(const long double x){
		if(varnames.size()!=1)
			return 0;
		Var v={*(varnames.begin()),x};
		return Evaluate({v});
	}
	Expression derive(string x){
		if(!built)
			build();
		vector<pair<Expression,Expression>> result;
		result.clear();
		for(auto it:parsed.v){
			if(it.type==Constant||(it.type==Variable&&it.value!=x)){
				Expression e1,e2;
				e1.v.push_back(it);
				token t={Constant,"0"};
				e2.v.push_back(t);
				result.push_back(make_pair(e1,e2));
			}
			else if(it.type==Variable&&it.value==x){
				Expression e1,e2;
				e1.v.push_back(it);
				token t={Variable,x,0,0,it.coef*it.pw,it.pw-1};
				if(it.pw==1)
					t={Constant,"1",1,it.coef*it.pw};
				e2.v.push_back(t);
				result.push_back(make_pair(e1,e2));
			}
			else if(it.type==Operation){
				if(it.value=="+"||it.value=="-"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-2].f);
					combine(e2,result[result.size()-2].s);
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].s);
					token t={Operation,it.value};
					e1.v.push_back(t);
					e2.v.push_back(t);
					result.pop_back();
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
				else if(it.value=="*"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-2].f);
					combine(e2,result[result.size()-2].f);
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].s);
					token t={Operation,"*"};
					e1.v.push_back(t);
					e2.v.push_back(t);
					combine(e2,result[result.size()-2].s);
					combine(e2,result[result.size()-1].f);
					e2.v.push_back(t);
					t={Operation,"+"};
					e2.v.push_back(t);
					result.pop_back();
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
				else if(it.value=="/"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-2].f);
					combine(e2,result[result.size()-2].s);
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].f);
					token t={Operation,"/"};
					e1.v.push_back(t);
					t={Operation,"*"};
					e2.v.push_back(t);
					combine(e2,result[result.size()-2].f);
					combine(e2,result[result.size()-1].s);
					e2.v.push_back(t);
					t={Operation,"-"};
					e2.v.push_back(t);
					combine(e2,result[result.size()-1].f);
					combine(e2,result[result.size()-1].f);
					t={Operation,"*"};
					e2.v.push_back(t);
					t={Operation,"/"};
					e2.v.push_back(t);
					result.pop_back();
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
				else if(it.value=="^"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-2].f);
					combine(e2,result[result.size()-2].f);
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].f);
					token t={Operation,"^"};
					e1.v.push_back(t);
					e2.v.push_back(t);
					combine(e2,result[result.size()-2].f);
					t={Function,"ln"};
					e2.v.push_back(t);
					combine(e2,result[result.size()-1].s);
					t={Operation,"*"};
					e2.v.push_back(t);
					combine(e2,result[result.size()-1].f);
					combine(e2,result[result.size()-2].s);
					t={Operation,"*"};
					e2.v.push_back(t);
					combine(e2,result[result.size()-2].f);
					t={Operation,"/"};
					e2.v.push_back(t);
					t={Operation,"+"};
					e2.v.push_back(t);
					t={Operation,"*"};
					e2.v.push_back(t);
					result.pop_back();
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
			}
			else if(it.type==Function){
				if(it.value=="sin"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].f);
					token t={Function,it.value};
					e1.v.push_back(t);
					t={Function,"cos"};
					e2.v.push_back(t);
					combine(e2,result[result.size()-1].s);
					t={Operation,"*"};
					e2.v.push_back(t);
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
				else if(it.value=="cos"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					token t={Constant,"0"};
					e2.v.push_back(t);
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].f);
					t={Function,it.value};
					e1.v.push_back(t);
					t={Function,"sin"};
					e2.v.push_back(t);
					combine(e2,result[result.size()-1].s);
					t={Operation,"*"};
					e2.v.push_back(t);
					t={Operation,"-"};
					e2.v.push_back(t);
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
				else if(it.value=="tan"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].s);
					token t={Function,it.value};
					e1.v.push_back(t);
					combine(e2,result[result.size()-1].f);
					combine(e2,result[result.size()-1].f);
					t={Operation,"*"};
					e2.v.push_back(t);
					t={Function,"cos"};
					e2.v.push_back(t);
					t={Operation,"/"};
					e2.v.push_back(t);
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
				else if(it.value=="ln"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].s);
					combine(e2,result[result.size()-1].f);
					token t={Function,it.value};
					e1.v.push_back(t);
					t={Operation,"/"};
					e2.v.push_back(t);
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
				else if(it.value=="sqrt"){
					Expression e1,e2;
					e1.v.clear();
					e2.v.clear();
					combine(e1,result[result.size()-1].f);
					combine(e2,result[result.size()-1].s);
					combine(e2,result[result.size()-1].f);
					token t={Function,"sqrt"};
					e1.v.push_back(t);
					e2.v.push_back(t);
					t={Constant,"2"};
					e2.v.push_back(t);
					t={Operation,"*"};
					e2.v.push_back(t);
					t={Operation,"/"};
					e2.v.push_back(t);
					result.pop_back();
					result.push_back(make_pair(e1,e2));
				}
			}
		}
		Func f(result[0].s);
		f.simplify();
		return f.parsed;
	}
	void simplify(){
		vector<Expression> v;
		for(int i=0;i<parsed.v.size();i++){
			auto it=parsed.v[i];
			if(it.type==Constant||it.type==Variable){
				Expression e;
				e.v.clear();
				e.v.push_back(it);
				v.push_back(e);
			}
			else if(it.type==Operation){
				if(it.value=="+"){
					Expression e1,e2=v.back();
					v.pop_back();
					e1=v.back();
					v.pop_back();
					if(e1.v.size()==1&&e1.v[0].type==Constant&&val(e1.v[0])==0)
						v.push_back(e2);
					else if(e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])==0)
						v.push_back(e1);
					else if(e1.v.size()==1&&e1.v[0].type==Constant&&e2.v.size()==1&&e2.v[0].type==Constant){
						Expression e=e1;
						e.v[0].eval=1;
						e.v[0].res=val(e1.v[0])+val(e2.v[0]);
						v.push_back(e);
					}
					else if(e1.v.size()==1&&e1.v[0].type==Variable&&e2.v.size()==1&&e2.v[0].type==Variable&&e1.v[0].value==e2.v[0].value&&e1.v[0].pw==e2.v[0].pw){
						Expression e=e1;
						e.v[0].coef+=e2.v[0].coef;
						v.push_back(e);
					}
					else{
						Expression e=e1;
						combine(e,e2);
						e.v.push_back(it);
						v.push_back(e);
					}
				}
				else if(it.value=="-"){
					Expression e1,e2=v.back();
					v.pop_back();
					e1=v.back();
					v.pop_back();
					if(e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])==0)
						v.push_back(e1);
					else if(e1.v.size()==1&&e1.v[0].type==Constant&&e2.v.size()==1&&e2.v[0].type==Constant){
						Expression e=e1;
						e.v[0].eval=1;
						e.v[0].res=val(e1.v[0])-val(e2.v[0]);
						v.push_back(e);
					}
					else if(e1.v.size()==1&&e1.v[0].type==Variable&&e2.v.size()==1&&e2.v[0].type==Variable&&e1.v[0].value==e2.v[0].value&&e1.v[0].pw==e2.v[0].pw){
						Expression e=e1;
						e.v[0].coef-=e2.v[0].coef;
						v.push_back(e);
					}
					else{
						Expression e=e1;
						combine(e,e2);
						e.v.push_back(it);
						v.push_back(e);
					}
				}
				else if(it.value=="*"){
					Expression e1,e2=v.back();
					v.pop_back();
					e1=v.back();
					v.pop_back();
					if(e1.v.size()==1&&e1.v[0].type==Constant&&val(e1.v[0])==0)
						v.push_back(e1);
					else if(e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])==0)
						v.push_back(e2);
					else if(e1.v.size()==1&&e1.v[0].type==Constant&&e2.v.size()==1&&e2.v[0].type==Constant){
						Expression e=e1;
						e.v[0].res=val(e1.v[0])*val(e2.v[0]);
						e.v[0].eval=1;
						v.push_back(e);
					}
					else if(e1.v.size()==1&&e1.v[0].type==Constant&&e2.v.size()==1&&e2.v[0].type==Variable){
						Expression e=e2;
						e.v[0].coef*=val(e1.v[0]);
						v.push_back(e);
					}
					else if(e1.v.size()==1&&e1.v[0].type==Variable&&e2.v.size()==1&&e2.v[0].type==Constant){
						Expression e=e1;
						e.v[0].coef*=val(e2.v[0]);
						v.push_back(e);
					}
					else if(e1.v.size()==1&&e1.v[0].type==Variable&&e2.v.size()==1&&e2.v[0].type==Variable&&e1.v[0].value==e2.v[0].value){
						Expression e=e1;
						e.v[0].coef*=e2.v[0].coef;
						e.v[0].pw+=e2.v[0].pw;
						v.push_back(e);
					}
					else if(e1.v.size()==1&&e1.v[0].type==Constant&&val(e1.v[0])==1)
						v.push_back(e2);
					else if(e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])==1)
						v.push_back(e1);
					else{
						Expression e=e1;
						combine(e,e2);
						e.v.push_back(it);
						v.push_back(e);
					}
				}
				else if(it.value=="/"){
					Expression e1,e2=v.back();
					v.pop_back();
					e1=v.back();
					v.pop_back();
					if(e1.v.size()==1&&e1.v[0].type==Constant&&val(e1.v[0])==0)
						v.push_back(e1);
					else if(e1.v.size()==1&&e1.v[0].type==Constant&&e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])!=0){
						Expression e=e1;
						e.v[0].eval=1;
						e.v[0].res=val(e1.v[0])/val(e2.v[0]);
						v.push_back(e);
					}
					else if(e2.v.size()==1&&e2.v[0].type==Variable){
						v.push_back(e1);
						Expression e=e2;
						e.v[0].pw*=-1;
						v.push_back(e);
						parsed.v[i].value='*';
						i--;
						continue;
					}
					else if(e1.v.size()==1&&e1.v[0].type==Variable&&e2.v.size()==1&&e2.v[0].type==Constant){
						Expression e=e1;
						e.v[0].coef/=val(e2.v[0]);
						v.push_back(e);
					}
					else if(e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])==1)
						v.push_back(e1);
					else{
						Expression e=e1;
						combine(e,e2);
						e.v.push_back(it);
						v.push_back(e);
					}
				}
				else if(it.value=="^"){
					Expression e1,e2=v.back();
					v.pop_back();
					e1=v.back();
					v.pop_back();
					if(e1.v.size()==1&&e1.v[0].type==Constant&&val(e1.v[0])==0)
						v.push_back(e1);
					else if(e1.v.size()==1&&e1.v[0].type==Constant&&e2.v.size()==1&&e2.v[0].type==Constant){
						Expression e=e1;
						e.v[0].eval=1;
						e.v[0].res=pow(val(e1.v[0]),val(e2.v[0]));
						v.push_back(e);
					}
					else if(e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])==1)
						v.push_back(e1);
					else if(e2.v.size()==1&&e2.v[0].type==Constant&&val(e2.v[0])==0){
						e2.v[0].eval=1;
						e2.v[0].res=1;
						v.push_back(e2);
					}
					else if(e1.v.size()==1&&e1.v[0].type==Variable&&e2.v.size()==1&&e2.v[0].type==Constant){
						Expression e=e1;
						e.v[0].pw*=val(e2.v[0]);
						v.push_back(e);
					}
					else{
						Expression e=e1;
						combine(e,e2);
						e.v.push_back(it);
						v.push_back(e);
					}
				}
				else{
					Expression e1,e2=v.back();
					v.pop_back();
					e1=v.back();
					v.pop_back();
					Expression e=e1;
					combine(e,e2);
					e.v.push_back(it);
					v.push_back(e);
				}
			}
			else{
				Expression e1=v.back();
				v.pop_back();
				e1.v.push_back(it);
				v.push_back(e1);
			}
			if(v.back().v.size()==1&&v.back().v[0].type==Variable&&v.back().v[0].pw==0){
				Expression e=v.back();
				e.v[0].type=Constant;
				e.v[0].value="1";
				e.v[0].eval=1;
				e.v[0].res=e.v[0].coef;
				e.v[0].pw=1;
				v.pop_back();
				v.push_back(e);
			}
			if(v.back().v.size()==1&&v.back().v[0].type==Variable&&fabs(v.back().v[0].coef)<1e-12){
				Expression e=v.back();
				e.v[0].type=Constant;
				e.v[0].value="0";
				e.v[0].eval=0;
				e.v[0].res=e.v[0].coef;
				e.v[0].pw=1;
				v.pop_back();
				v.push_back(e);
			}
			if(v.back().v.size()==1&&v.back().v[0].type==Constant&&fabs(val(v.back().v[0]))<1e-12){
				Expression e=v.back();
				e.v[0].type=Constant;
				e.v[0].value="0";
				e.v[0].eval=0;
				e.v[0].res=e.v[0].coef;
				e.v[0].pw=1;
				v.pop_back();
				v.push_back(e);
			}
		}
		parsed.v.clear();
		for(auto &it:v)
			combine(parsed,it);
		return;
	}
};
struct lineareq{
	string expression;
	vector<pair<string,long double>> coef;
	long double ans;
	lineareq(){
	}
	lineareq(string e){
		expression=e;
		build();
	}
	void build(){
		long double tmp=1;
		bool neg=0;
		bool num=0;
		int n=expression.size();
		for(int i=0;i<expression.size();i++){
            if(expression[i]==' ' || expression[i]=='\n')continue;
			if(isdigit(expression[i])){
				string s="";
				while(i<n&&(isdigit(expression[i])||expression[i]=='.'||expression[i]=='e'))
					s+=expression[i++];
				i--;
				tmp=stold(s);
				num=1;
				continue;
			}
			if(expression[i]=='+'){
				neg=0;
				continue;
			}
			if(expression[i]=='-'){
				neg=1;
				continue;
			}
			if(isupper(expression[i])){
				string s="";
				while(i<n&&(isalpha(expression[i])||isdigit(expression[i])))
					s+=expression[i++];
				coef.push_back({s,tmp*(neg?-1:1)});
				i--;
				tmp=1;
				neg=0;
				num=0;
				continue;
			}
			if(expression[i]=='='){
				if(num)
					ans=tmp*(neg?-1:1);
				num=0;
				neg=0;
				tmp=1;
				continue;
			}
		}
		if(num)
			ans=tmp*(neg?-1:1);
	}
};
bool valid,converge;
int itcount;
vector<Var> GaussSeidel(vector<string> vs,vector<Var> vars,long double eps=1e-10){
	valid=1;
	error=0;
	converge=1;
	if(vs.size()!=vars.size()){
		valid=0;
		return vars;
	}
	vector<pair<string,Func>> vec;
	map<string,long double> mp;
	for(auto it:vars)
		mp[it.name]=it.value;
	set<string> st;
	for(int i=0;i<vs.size();i++){
		lineareq l(vs[i]);
		long double sum=0;
		for(int i=0;i<l.coef.size();i++)
			sum+=fabs(l.coef[i].s);
		int idx=-1;
		for(int i=0;i<l.coef.size();i++){
			if(fabs(l.coef[i].s)*2>sum){
				int z=st.size();
				st.insert(l.coef[i].f);
				if(st.size()==z){
					converge=0;
					break;
				}
				idx=i;
			}
		}
		for(int i=0;idx==-1&&i<l.coef.size();i++){
			if(st.find(l.coef[i].f)==st.end()){
				idx=i;
				st.insert(l.coef[i].f);
				break;
			}
		}
		if(idx==-1){
			valid=0;
			return vars;
		}
		Expression ex;
		ex.v.clear();
		ex.v.push_back({Constant,"",1,l.ans});
		bool bol=0;
		for(int i=0;i<l.coef.size();i++){
			if(i==idx)
				continue;
			ex.v.push_back({Variable,l.coef[i].f,0,0,-l.coef[i].s});
			ex.v.push_back({Operation,"+"});
			bol=1;
		}
		ex.v.push_back({Constant,"",1,l.coef[idx].s});
		ex.v.push_back({Operation,"/"});
		Func f(ex);
		vec.push_back({l.coef[idx].f,f});
	}
	while(converge||itcount<1000){
		long double mx=0;
		for(int i=0;i<vec.size();i++){
			long double x=vec[i].s.Evaluate(mp);
			mx=max(mx,fabs(x-mp[vec[i].f]));
			mp[vec[i].f]=x;
		}
		itcount++;
		if(mx<eps)
			break;
	}
	for(auto &it:vars)
		it.value=mp[it.name];
	return vars;
}
vector<Var> Jacobi(vector<string> vs,vector<Var> vars,long double eps=1e-10){
	valid=1;
	error=0;
	converge=1;
	if(vs.size()!=vars.size()){
		valid=0;
		return vars;
	}
	vector<pair<string,Func>> vec;
	map<string,long double> mp;
	for(auto it:vars)
		mp[it.name]=it.value;
	set<string> st;
	for(int i=0;i<vs.size();i++){
		lineareq l(vs[i]);
		long double sum=0;
		for(int i=0;i<l.coef.size();i++)
			sum+=fabs(l.coef[i].s);
		int idx=-1;
		for(int i=0;i<l.coef.size();i++){
			if(fabs(l.coef[i].s)*2>sum){
				int z=st.size();
				st.insert(l.coef[i].f);
				if(st.size()==z){
					converge=0;
					break;
				}
				idx=i;
			}
		}
		for(int i=0;idx==-1&&i<l.coef.size();i++){
			if(st.find(l.coef[i].f)==st.end()){
				idx=i;
				st.insert(l.coef[i].f);
				break;
			}
		}
		if(idx==-1){
			valid=0;
			return vars;
		}
		Expression ex;
		ex.v.clear();
		ex.v.push_back({Constant,"",1,l.ans});
		bool bol=0;
		for(int i=0;i<l.coef.size();i++){
			if(i==idx)
				continue;
			ex.v.push_back({Variable,l.coef[i].f,0,0,-l.coef[i].s});
			ex.v.push_back({Operation,"+"});
			bol=1;
		}
		ex.v.push_back({Constant,"",1,l.coef[idx].s});
		ex.v.push_back({Operation,"/"});
		Func f(ex);
		vec.push_back({l.coef[idx].f,f});
	}
	while(converge||itcount<1000){
		long double mx=0;
		vector<pair<string,long double>> vv;
		for(int i=0;i<vec.size();i++){
			long double x=vec[i].s.Evaluate(mp);
			mx=max(mx,fabs(x-mp[vec[i].f]));
			vv.push_back({vec[i].f,x});
		}
		for(auto it:vv)
			mp[it.f]=it.s;
		itcount++;
		if(mx<eps)
			break;
	}
	for(auto &it:vars)
		it.value=mp[it.name];
	return vars;
}
Func Lagrange(vector<pair<long double,long double>> vec,string var="X"){
	valid=1;
	error=0;
	vector<long double> ans(vec.size());
	for(auto &it:ans)
		it=0;
	for(int i=0;i<vec.size();i++){
		long double co=vec[i].s;
		for(int j=0;j<vec.size();j++){
			if(i==j)
				continue;
			if(vec[i].f==vec[i].s){
				valid=0;
				Func f("0");
				return f;
			}
			co/=vec[i].f-vec[j].f;
		}
		vector<long double> ad={1};
		for(int j=0;j<vec.size();j++){
			if(i==j)
				continue;
			ad.push_back(ad.back());
			for(int k=ad.size()-2;k>0;k--)
				ad[k]=ad[k]*(-vec[j].f)+ad[k-1];
			ad[0]*=(-vec[j].f);
		}
		for(int i=0;i<ad.size();i++)
			ans[i]+=ad[i]*co;
	}
	Expression ex;
	ex.v.clear();
	for(int i=0;i<ans.size();i++){
		ex.v.push_back({Variable,var,0,0,ans[i],i});
		if(i)
			ex.v.push_back({Operation,"+"});
	}
	Func f(ex);
	f.simplify();
	return f;
}

long double bisection(Func f,long double a,long double b,long double eps=1e-10){
	valid=1;
	itcount=0;
	if(f(a)==0)
		return a;
	if(f(b)==0)
		return b;
	if(f(a)>0)
		swap(a,b);
	if(f(a)>0){
		valid=0;
		return 0;
	}
	while(fabs(a-b)>eps){
		itcount++;
		long double mid=(a+b)/2;
		if(f(mid)<0)
			a=mid;
		else
			b=mid;
	}
	return (a+b)/2;
}

long double secant(Func f,long double a,long double b,long double eps=1e-10){
	itcount=0;
	error=0;
	valid=1;
	if(f(a)==0)
		return a;
	if(f(b)==0)
		return b;
	if(f(a)*f(b)>=0){
		valid=0;
		return 0;
	}
	while(fabs(a-b)>eps && itcount<1e6){
		itcount++;
		long double nxt=(a*f(b)-b*f(a))/(f(b)-f(a));
		a=b;
		b=nxt;
	}
	if(fabs(a-b)>eps)valid=0;
	return b;
}
long double newton(Func f,long double x,string solvefor="X",long double eps=1e-10){
	itcount=0;
	valid=1;
	converge=1;
	Func f1(f.derive(solvefor));
	Func f2(f1.derive(solvefor));
	if(fabs(f(x)*f2(x))>=f1(x)*f1(x))
		converge=0;
	double prv=0;
	while((itcount==0||fabs(x-prv)>eps)&&(converge||itcount<100)){
		double nxt=x-f(x)/f1(x);
		prv=x;
		x=nxt;
		itcount++;
	}
	if(fabs(x-prv)>eps)
		valid=0;
	return x;
}
long double midpoint(Func f,long double a,long double b,int parts=1){
	long double ans=0;
	long double h=(b-a)/parts;
	for(int i=0;i<parts;i++)
		ans+=f(a+i*h+h/2);
	ans*=h;
	return ans;
}
long double trapezoidal(Func f,long double a,long double b,int parts=1){
	long double ans=0;
	long double h=(b-a)/parts;
	ans+=f(a)+f(b);
	for(int i=1;i<parts;i++)
		ans+=2*f(a+i*h);
	ans*=h/2;
	return ans;
}
long double simpsons(Func f,long double a,long double b,int parts=1){
	long double ans=0;
	long double h=(b-a)/parts;
	ans+=f(a)-f(b);
	for(int i=1;i<=parts;i++){
		ans+=4*f(a+i*h-h/2);
		ans+=2*f(a+i*h);
	}
	ans*=h/6;
	return ans;
}
void print(Func f){
	for(auto it:f.parsed.v){
		if(it.type==Constant)
    		cout<<val(it)<<" ";
    	else{
    		if(it.coef!=1)
    			cout<<it.coef;
    		cout<<it.value;
    		if(it.pw!=1)
    			cout<<"^"<<it.pw;
    		cout<<" ";
    	}
	}
	cout<<"\n";
	return;
}
vector<Func>global_fs(100);
void printInfix(Func f)
{
	f.simplify();
	stack<string> st;
	for(auto it:f.parsed.v){
		if(it.type==Constant){
            stringstream termss;
            termss<<val(it);
			st.push(termss.str());
		}
		else if(it.type==Variable){
            stringstream termss;

			string term = "";
			if(it.coef!=1)termss<<(it.coef);
			termss<<it.value;
			if(it.pw!=1){termss<<"^";termss<<(it.pw);}
			st.push(termss.str());
		}
		else if(it.type==Function){
			string x=st.top();
			st.pop();
			if(it.value=="sin")
				st.push("sin("+x+")");
			else if(it.value=="cos")
				st.push("cos("+x+")");
			else if(it.value=="tan")
				st.push("tan("+x+")");
			else if(it.value=="ln"){
				st.push("ln("+x+")");
			}
			else if(it.value=="sqrt"){
				st.push("sqrt("+x+")");
			}
		}
		else if(it.type==Operation){
			string x,y=st.top();
			st.pop();
			x=st.top();
			st.pop();
			if(it.value=="+")
				st.push("("+x+"+"+y+")");
			else if(it.value=="-")
				st.push("("+x+"-"+y+")");
			else if(it.value=="*")
				st.push("("+x+"*"+y+")");
			else if(it.value=="/"){
				st.push("("+x+"/"+y+")");
			}
			else if(it.value=="^")
				st.push("("+x+"^"+y+")");
		}
	}
	cout<<st.top()<<endl;
}
void storeFunction(int id=-1)
{
	if(id==-1)
	{
		cout<<"index to store the function: ";
		cin>>id;
		if(id>99 || id<0)
		{
			cout<<"Please enter a valid index next time..\n";
			return;
		}
	}
	if(global_fs[id].built)
	{
		cout<<"You are about to override a stored function, proceed? (Y/N)";
		string s;
		cin>>s;
		if(s=="Y")
		{
			goto slv;
		}
		else if(s=="N")
		{
			cout<<"Ok...\n";
			return;
		}
		else
		{
			cout<<"Invalid choice..\n";
			return;
		}
	}
	slv:;
	cout<<"f(X) = ";
	string s="";
	while(s.size()==0 || s[0]=='\n')
		getline(cin,s);

	Func f = Func(s);
	if(f.built==0)
	{
		cout<<"Error, please try inputting the function correctly\n";
		storeFunction();
	}
	else
	{
		global_fs[id]=f;
		cout<<"Stored f(X) correctly\n";
	}
}
void evaluateFx()
{
	cout<<"Function index: ";
	int id;
	cin>>id;
	if(id<0||id>99)
	{
		cout<<"Please enter a valid index\n";
		evaluateFx();
		return;
	}

	if(global_fs[id].built==0)
	{
		cout<<"No stored function at index "<<id<<"\n";
		return;
	}
	cout<<"X = ";
	long double x;
	cin>>x;
	cout<<"f("<<x<<") = "<<global_fs[id](x)<<endl;
	cout<<endl;

}
void printFunction()
{
	int id;
	cout<<"Function index: ";
	cin>>id;
	if(id<0||id>99)
	{
		cout<<"Please enter a valid index\n";
		printFunction();
		return;
	}

	if(global_fs[id].built==0)
	{
		cout<<"No stored function at index "<<id<<"\n";
		return;
	}
	cout<<"f(X) = ";
	printInfix(global_fs[id]);
	cout<<endl;
	cout<<endl;
}
void deriveFunction()
{
	cout<<"f(X) index: ";
	int id;
	cin>>id;
	if(id<0||id>99)
	{
		cout<<"Please enter a valid index\n";
		return;
	}
	if(global_fs[id].built==0)
	{
		cout<<"No stored function at index "<<id<<"\n";
		return;
	}
	int n;
	cout<<"n = ";
	cin>>n;
	if(n<1)
	{
		cout<<"can't integrate..\n";
		return;
	}


	cout<<"store f`"<<n<<"(X) at index: ";
	int idxs;
	cin>>idxs;
	if(idxs<0||idxs>99)
	{
		cout<<"\nInvalid index\n";
		return;
	}
	if(global_fs[idxs].built)
	{
		cout<<"You are about to override a stored function, proceed? (Y/N)";
		string s;
		cin>>s;
		if(s=="Y")
		{
			goto slv;
		}
		else if(s=="N")
		{
			cout<<"Ok...\n";
			return;
		}
		else
		{
			cout<<"Invalid choice..\n";
			return;
		}
	}
	slv:;
	global_fs[idxs] = global_fs[id].derive("X");
	global_fs[idxs].simplify();
	for(int i=1;i<n;i++)
	{
		global_fs[idxs]=global_fs[idxs].derive("X");
		global_fs[idxs].simplify();
	}
	cout<<"Done.\n";


}
void appFxe0()
{
	cout<<"f(X) index: ";
	int id;
	cin>>id;
	if(id<0||id>99)
	{
		cout<<"Please enter a valid index\n";
		return;
	}
	if(global_fs[id].built==0)
	{
		cout<<"No stored function at index "<<id<<"\n";
		return;
	}
	cout<<"1- Bisection\n";
	cout<<"2- Secant\n";
	cout<<"3- Newton\n";
	int choice;
	cin>>choice;
	if(choice<0 || choice>3){
		cout<<"Invalid choice\n";
		return;
	}
	long double l,r,initial;
	if(choice == 1 || choice == 2)
	{
		cout<<"L = ";
		cin>>l;
		cout<<"R = ";
		cin>>r;
	}
	else
	{
		cout<<"X0 = ";
		cin>>initial;
	}
	long double eps;
	cout<<"eps = ";
	cin>>eps;

	if(choice == 1)
	{
		long double res = bisection(global_fs[id],l,r,eps);
		if(!valid || error){
            cout<<"Invalid\n";
            return;
		}
		cout<<"X =~ "<<res<<endl;
		return;
	}
	if(choice == 2)
	{
		long double res = secant(global_fs[id],l,r,eps);
		if(!valid || error){
            cout<<"Invalid\n";
            return;
		}
		cout<<"X =~ "<<res<<endl;
		return;
	}
	long double res = newton(global_fs[id],initial,"X",eps);
	if(!converge)cout<<"Does not converge btw\n";
	if(!valid || error){
        cout<<"Invalid\n";
        return;
    }
	cout<<"X =~ "<<res<<endl;
	return;
}
void appIntegral()
{
	cout<<"f(X) index: ";
	int id;
	cin>>id;
	if(id<0||id>99)
	{
		cout<<"Please enter a valid index\n";
		return;
	}
	if(global_fs[id].built==0)
	{
		cout<<"No stored function at index "<<id<<"\n";
		return;
	}
	cout<<"1- Midpoint basic\n";
	cout<<"2- Midpoint composite\n";
	cout<<"3- Trapezoidal basic\n";
	cout<<"4- Trapezoidal composite\n";
	cout<<"5- Simpsons basic\n";
	cout<<"6- Simpsons composite\n";
	int choice;
	cin>>choice;
	if(choice<0 || choice>6){
		cout<<"Invalid choice\n";
		return;
	}
	cout<<"A: ";
	long double a;
	cin>>a;
	cout<<"B: ";
	long double b;
	cin>>b;
	int parts=1;
	if(choice%2 == 0)
	{
		cout<<"Parts = ";
		cin>>parts;
	}

	if(choice == 1 || choice==2)
	{
		cout<<"Integration result = "<<midpoint(global_fs[id],a,b,parts)<<endl;
	}
	else if(choice == 3 || choice== 4)
	{
		cout<<"Integration result = "<<trapezoidal(global_fs[id],a,b,parts)<<endl;
	}
	else if(choice == 5 || choice==6)
	{
		cout<<"Integration result = "<<simpsons(global_fs[id],a,b,parts)<<endl;
	}

}
void sysOfEq()
{
    cout<<"1- Jacobi\n";
    cout<<"2- Gauss-Seidel\n";
    int ch;
    cin>>ch;
    if(ch<1 || ch>2)
    {
    	cout<<"Invalid choice\n";
		return;
    }
    cout<<"Number of equations: ";
    int n;
    cin>>n;
    vector<Var>v;
    cout<<"Initial values (of the form 'Name' 'Value', for example A 72)\n";
    for(int i=0;i<n;i++)
    {
        string s;
        long double d;
        cin>>s>>d;
        v.push_back({s,d});
    }
    vector<string>eq;
    cout<<"Enter the equations (c1A +c2B + c3C = c4) :\n";
    for(int i=0;i<n;i++)
    {
        string temp;
        while(1)
        {
            getline(cin,temp);
            if(temp.size()==0 || temp[0]=='\n')continue;
            break;
        }
        eq.push_back(temp);
    }
    cout<<"eps = ";
    long double eps;
    cin>>eps;
    vector<Var>res;
    if(ch==1)res = Jacobi(eq,v,eps);
    else res = GaussSeidel(eq,v,eps);
    if(!converge)cout<<"Does not converge btw\n";
	if(!valid || error){
        cout<<"Invalid\n";
        return;
    }
    for(auto i:res)
        cout<<i.name<<" = "<<i.value<<endl;
}
void interpolate()
{
    cout<<"Number of points: ";
    int n;
    cin>>n;
    vector<pair<long double,long double>>v;
    cout<<"list of X f(X):\n";
    for(int i=0;i<n;i++)
    {
        long double a,b;
        cin>>a>>b;
        v.push_back({a,b});
    }
    Func f = Lagrange(v);
    if(!valid || error){
        cout<<"Invalid\n";
        return;
    }
    cout<<"store f(X) at index: ";
	int idxs;
	cin>>idxs;
	if(idxs<0||idxs>99)
	{
		cout<<"\nInvalid index\n";
		return;
	}
	if(global_fs[idxs].built)
	{
		cout<<"You are about to override a stored function, proceed? (Y/N)";
		string s;
		cin>>s;
		if(s=="Y")
		{
			goto slv;
		}
		else if(s=="N")
		{
			cout<<"Ok...\n";
			return;
		}
		else
		{
			cout<<"Invalid choice..\n";
			return;
		}
	}
	slv:;
	global_fs[idxs]=f;
	cout<<"Done\n";
	return;
}

void printMainMenu()
{
	cout<<"What do you want to do?\n";
	cout<<"1- Store a function f(X)\n";
	cout<<"2- Print a stored function\n";
	cout<<"3- Evaluate f(X)\n";
	cout<<"4- N-th derivative of f(X)\n";
	cout<<"5- Approximate solution for an equation f(X) = 0\n";
	cout<<"6- Approximate integral for f(X)\n";
	cout<<"7- System of equations solver\n";
	cout<<"8- Interpolation\n";
	cout<<"0- Exit\n";
	string choice;
	cin>>choice;
	system("CLS");
	if(choice == "1")
	{
		storeFunction();
	}
	else if(choice == "2")
	{
		printFunction();
	}
	else if(choice == "3")
	{

		evaluateFx();

	}
	else if(choice == "4")
	{
		deriveFunction();
	}
	else if(choice == "5")
	{
		appFxe0();
	}
	else if(choice == "6")
	{
		appIntegral();

	}
	else if(choice == "7")
	{
        sysOfEq();
	}
	else if(choice == "8")
	{
        interpolate();
	}
	else if(choice == "0")
	{
		cout<<"Ok bye!\n";
		exit(0);
	}
	else
	{
		cout<<"Invalid choice, please try again\n";
	}


}
int main(){
    	/*
    	Code can read an expression and convert it to postfix
    	evaluate a function at a certain point
    	derive a function



    	implemented functions


    	bisection
    	secant
    	newton

    	jacobi
        gauss

        lagrange

        mid point
    	trapezoidal
    	simpsons



    	*/
std::cout << fixed << std::setprecision(20);
    while(1)
        printMainMenu();

    return 0;
}