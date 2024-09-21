import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Partitioner;

public class GenrePartitioner extends Partitioner<Text, MovieWritable> {
    public int getPartition(Text key, MovieWritable value, int numPartitions) {   
        return (key.toString().hashCode() & Integer.MAX_VALUE) % numPartitions;
    }
}

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.partition.HashPartitioner;

public class MovieAnalysisDriver extends Configured implements Tool {
    public int run(String[] args) throws Exception {
        if (args.length != 3) {
            System.err.println("Usage: MovieAnalysisDriver <input path> <output path> <revenue threshold>");
            System.exit(-1);
        }

        Job job = Job.getInstance(getConf(), "Movie Analysis");
        job.setJarByClass(MovieAnalysisDriver.class);

        job.setMapperClass(MovieMapper.class);
        job.setReducerClass(MovieReducer.class);
        job.setPartitionerClass(GenrePartitioner.class);
        job.setNumReduceTasks(3);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(MovieWritable.class);

        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));

        job.getConfiguration().setDouble("revenue.threshold", Double.parseDouble(args[2]));

        return job.waitForCompletion(true) ? 0 : 1;
    }

    public static void main(String[] args) throws Exception {
        int exitCode = ToolRunner.run(new MovieAnalysisDriver(), args);
        System.exit(exitCode);
    }
}


import org.apache.hadoop.io.DoubleWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.io.WritableComparable;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

public class MovieWritable implements Writable {
    private Text title;
    private DoubleWritable rating;
    private DoubleWritable revenue;

    public MovieWritable() {
        this.title = new Text();
        this.rating = new DoubleWritable();
        this.revenue = new DoubleWritable();
    }

    public MovieWritable(String title, double rating, double revenue) {
        this.title = new Text(title);
        this.rating = new DoubleWritable(rating);
        this.revenue = new DoubleWritable(revenue);
    }
    public void write(DataOutput out) throws IOException {
        title.write(out);
        rating.write(out);
        revenue.write(out);
    }
    public void readFields(DataInput in) throws IOException {
        title.readFields(in);
        rating.readFields(in);
        revenue.readFields(in);
    }

    public Text getTitle() {
        return title;
    }

    public DoubleWritable getRating() {
        return rating;
    }

    public DoubleWritable getRevenue() {
        return revenue;
    }
}


import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import java.io.IOException;

public class MovieMapper extends Mapper<LongWritable, Text, Text, MovieWritable> {
    private double revenueThreshold;
    protected void setup(Context context) {
        revenueThreshold = context.getConfiguration().getDouble("revenue.threshold", 0.0);
    }
    protected void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
        String[] fields = value.toString().split(";");
        String title = fields[1];
        String genres = fields[2];
        double rating = Double.parseDouble(fields[3]);
        double revenue = Double.parseDouble(fields[4]);

        if (revenue > revenueThreshold) {
            for (String genre : genres.split("\\|")) {
                context.write(new Text(genre), new MovieWritable(title, rating, revenue));
            }
        }
    }
}

public class MovieReducer extends Reducer<Text, MovieWritable, Text, Text> {
    protected void reduce(Text key, Iterable<MovieWritable> values, Context context) throws IOException, InterruptedException {
        int count = 0;
        double totalRating = 0.0;

        for (MovieWritable value : values) {
            totalRating += value.getRating().get();
            count++;
        }
                                               
        double averageRating = totalRating / count;
        context.write(key, new Text("Average Rating: " + averageRating));
    }
}