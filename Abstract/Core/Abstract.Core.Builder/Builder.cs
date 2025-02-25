﻿using System.Text;
using Abstract.Core.Src;
using Abstract.InterOp;


namespace Abstract.Core.Builder;

/// <summary>
/// Build namespace.
/// Countains interoperable structures to help
/// building abstract solutions inside abstract
/// code.
/// </summary>
[AbstractNamespace("Build")]
public static class BuildNamespace
{
    /// <summary>
    /// A abstract build step
    /// </summary>
    [AbstractStruct("Step")]
    public interface IStep
    {
        [AbstractFunction]
        public void Run();
    }

    /// <summary>
    /// A build context
    /// </summary>
    [AbstractStruct("Builder")]
    public interface IBuilder
    {

        

    }

    /// <summary>
    /// Reference to a generated executable
    /// </summary>
    [AbstractStruct("Executable")]
    public interface IExecutable
    {
        
    }

    /// <summary>
    /// Node to show process state
    /// </summary>
    [AbstractStruct("Step")]
    public class Progress(string name)
    {

        private string name = name;
        private int total = 1;
        private int completed = 0;
        private bool done = false;

        private Progress _parent = null!;
        private List<Progress> _children = [];

        public string Name => name;
        public int Total => total;
        public int Completed => completed;

        public Progress Branch(string name, int initialTotal = 0)
        {
            var p = new Progress(name)
            {
                _parent = this,
                total = initialTotal
            };
            _children.Add(p);
            return p;
        }
       
        public void Done()
        {
            done = true;
            _children.Clear();
        }
        public void SetCompleted(int value) => completed = value;
        public void SetTotal(int value) => total = value;
        public void CompleteOne() => completed++;

        public void End() => _parent._children.Remove(this);

        public bool IsAllDone() => _children.All(e => e.done);
        public bool IsComplete() => total == completed;

        public override string ToString()
        {
            var str = new StringBuilder();

            str.AppendLine($"{name} " + (done ? "\x1b[42;30mDONE\x1b[0m" : (total > 0 ? $"[{completed}/{total}]" : "")));
            foreach (var i in  _children.ToArray())
            {
                var l = i.ToString().Split(Environment.NewLine);
                foreach (var j in l) str.AppendLine($"\t{j}");
            }
            str.Length -= Environment.NewLine.Length;

            return str.ToString();
        }
    }

}
