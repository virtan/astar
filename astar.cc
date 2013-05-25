#include <iostream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <queue>
#include <string.h>
#include <assert.h>

using namespace std;

class point {
    public:
        enum random_t { random };

    public:
        point() {}
        point(const char *str) {
            strncpy(word, str, 5);
        }
        point(random_t) {
            for(size_t i = 0; i < word_size; ++i)
                word[i] = 'a' + lrand48() % ('z' - 'a' + 1);
        }
        point(const char w[5], size_t i, char c) {
            memcpy(word, w, word_size);
            word[i] = c;
        }
        vector<point> neighbors() const {
            vector<point> res;
            res.reserve(word_size * ('z' - 'a' + 1));
            for(size_t i = 0; i < word_size; ++i)
                for(char s = 'a'; s <= 'z'; ++s)
                    if(word[i] != s)
                        res.emplace_back(point(word, i, s));
            return res;
        }
        size_t difference(const point &b) const {
            size_t res = 0;
            for(size_t i = 0; i < word_size; ++i)
                res += (size_t) (word[i] != b.word[i]);
            return res;
        }
        bool operator==(const point &b) const {
            return strncmp(word, b.word, word_size) == 0;
        }

        bool operator>(const point &b) const {
            return strncmp(word, b.word, word_size) > 0;
        }


    private:
        static const size_t word_size = 5;
        char word[word_size];

    friend ostream &operator<<(ostream&, const point&);
    friend struct std::hash<point>;
};

ostream &operator<<(ostream &os, const point &p) {
    os.write(p.word, p.word_size);
    return os;
}

namespace std {
    template<> struct hash<point> {
        size_t operator()(const point &a) const {
            return _Hash_impl::hash(a.word, a.word_size);
        }
    };
}

class space : public unordered_set<point> {
    public:
        enum random_t { random };

    public:
        space() {}
        space(random_t, size_t sz) {
            while(sz-- > 0)
                insert(point(point::random));
        }
};

class astar {
    private:
        struct vertex {
            vertex() {}
            vertex(const point &_pt) : pt(_pt), steps_done(0), steps_estimate(0), weight(0) {}
            struct hash {
                size_t operator()(const astar::vertex &v) const {
                    return _Hash_impl::hash((const char*) &v, sizeof(v));
                }
            };
            struct weight_less {
                bool operator()(const vertex &a, const vertex &b) const {
                    return a.weight != b.weight ? a.weight > b.weight : a.pt > b.pt;
                }
            };
            bool operator==(const vertex &b) const {
                return pt == pt && steps_done == b.steps_done &&
                    steps_estimate == b.steps_estimate && weight == b.weight;
            }
            point pt;
            size_t steps_done;
            size_t steps_estimate;
            size_t weight;
        };

    public:
        static int path(const point &from, const point &to, const space &sp) {
            priority_queue<vertex, vector<vertex>, vertex::weight_less> heap;
            unordered_map<point, vertex> open, closed;
            vertex start(from);
            start.weight = start.steps_estimate = cost_estimate(from, to);
            open[from] = start;
            heap.push(start);
            while(!heap.empty()) {
                const vertex &top = heap.top();
                top == heap.top();
                //cerr << "\rtop params " << top.pt << " " << top.steps_done << " "
                //     << top.steps_estimate << " " << top.weight << "     ";
                if(top.pt == to) {
                    return (int) (open[to].steps_done);
                }
                if(closed.find(top.pt) != closed.end()) {
                    heap.pop();
                    continue;
                }
                auto vi = open.find(top.pt);
                assert(vi != open.end());
                auto ci = closed.insert(*vi).first;
                open.erase(vi);
                heap.pop();
                vector<point> neighbors = get_neighbors(ci->first, sp, to);
                for(auto &newpoint : neighbors) {
                    if(closed.find(newpoint) != closed.end()) continue;
                    vertex newvertex(newpoint);
                    newvertex.steps_done = ci->second.steps_done + 1;
                    newvertex.steps_estimate = cost_estimate(newpoint, to);
                    newvertex.weight = newvertex.steps_done + newvertex.steps_estimate;
                    auto existingi = open.find(newpoint);
                    if(existingi == open.end() || existingi->second.weight > newvertex.weight) {
                        heap.push(newvertex);
                        open[newpoint] = newvertex;
                    }
                }
            }
            return -1;
        }
        
        static vector<point> get_neighbors(const point &target, const space &sp, const point &to) {
            vector<point> res;
            for(point &p : target.neighbors())
                if(sp.find(p) != sp.end() || p == to)
                    res.emplace_back(p);
            return res;
        }

        static size_t cost_estimate(const point &a, const point &b) {
            return a.difference(b);
        }
};


int main() {
    srand48(time(NULL));
    size_t set_size = 500000;
    space sp(space::random, set_size);
    for(int i = 0; i < 100; ++i) {
        space fromto(space::random, 2);
        cout << "distance between " << *fromto.begin() << " and " << *(++fromto.begin())
             << " on set of size " << set_size << " is "
             << astar::path(*fromto.begin(), *(++fromto.begin()), sp) << endl;
    }
    /*
    space sp;
    sp.insert("start"); sp.insert("stark"); sp.insert("stack"); sp.insert("slack");
    sp.insert("bluck"); sp.insert("black"); sp.insert("blank"); sp.insert("bland");
    sp.insert("aland"); sp.insert("cland"); sp.insert("brand"); sp.insert("braid");
    cout << "distance between smart and brain is " << astar::path("smart", "brain", sp) << endl;
    */
    return 0;
}
