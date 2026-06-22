package cs305.minesweeper.gui;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DecimalFormat;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import cs305.minesweeper.MinesweeperBoard;
import cs305.minesweeper.javamine.GameBoard;
import cs305.minesweeper.model.LogicModel;
import cs305.minesweeper.winmine.WinMineBoard;

/**
 * Main driver for the minesweeper project.
 * @author Larry Brewer
 *
 */
public class Driver extends JFrame implements ActionListener{
	/*
	 * Graphical User Interface Components.
	 */
	private JButton start;
	private JRadioButton winMine, walterMine;
	private ButtonGroup mineGroup;
	private JMenuBar menuBar;
	private JMenu optionsMenu;
	private JMenuItem waltersDefaults, loopAndDelayOptions;
	private JLabel games, gamesLabel, wins, winsLabel, loss, lossLabel, 
					winPercentage, winPercentageLabel;
	private JPanel infoPanel, northPanel, optionPanel;
	
	/*
	 * Statistic Variables.
	 */
	private int numWins, numLosses, numGames = 0; 
	private double numWinPercentage = 0;
	
	/*
	 * Settings Variables.
	 */
	private int rows = 9;
	private int columns = 9;
	private int bomb = 10;
	private int loop = 1;
	private LogicModel lm = new LogicModel();
	private MinesweeperBoard mb = new GameBoard(rows,columns,bomb);
	private boolean hasBeenPlayed = false;
	private boolean hasChangedBoardType = true;
	/**
	 * Default constructor. Initializes components and 
	 * displays the frame
	 */
	public Driver(){
		initializeGuiComponents();
		initializeOptionsMenu();
		addComponentsToFrameAndDisplay();
		lm.setBoard(mb);
	}
	
	/**
	 * Initializes each GUI component.
	 */
	private void initializeGuiComponents() {
		northPanel = new JPanel(new GridLayout(1,2));

		start = new JButton("Start");
		start.addActionListener(this);

		optionPanel = new JPanel(new GridLayout(2,1));
		mineGroup = new ButtonGroup();

		winMine = new JRadioButton("Windows Minesweeper");
		mineGroup.add(winMine);
		optionPanel.add(winMine);
		winMine.addActionListener(this);

		walterMine = new JRadioButton("Walter's Minesweeper");
		mineGroup.add(walterMine);
		optionPanel.add(walterMine);
		northPanel.add(optionPanel);
		walterMine.addActionListener(this);
		walterMine.setSelected(true);

		infoPanel = new JPanel(new GridLayout(4,2));

		gamesLabel = new JLabel("Number of Games");
		games = new JLabel(numGames + "");
		infoPanel.add(gamesLabel);
		infoPanel.add(games);

		winsLabel = new JLabel("Number of Wins");
		wins = new JLabel(numWins + "");
		infoPanel.add(winsLabel);
		infoPanel.add(wins);

		lossLabel = new JLabel("Number of losses");
		loss = new JLabel(numLosses + "");
		infoPanel.add(lossLabel);
		infoPanel.add(loss);

		winPercentageLabel = new JLabel("Win Percentage");
		winPercentage = new JLabel(numWinPercentage + "");
		infoPanel.add(winPercentageLabel);
		infoPanel.add(winPercentage);

		northPanel.add(infoPanel);

		setDefaultCloseOperation(EXIT_ON_CLOSE);
	}

	/**
	 * Initializes the Options Menu.
	 */
	private void initializeOptionsMenu() {
		menuBar = new JMenuBar();
		optionsMenu = new JMenu("Options");
		
		waltersDefaults = new JMenuItem("Walter's Minesweeper Options");
		waltersDefaults.addActionListener(this);
		optionsMenu.add(waltersDefaults);

		loopAndDelayOptions= new JMenuItem("Loop/Delay Options");
		loopAndDelayOptions.addActionListener(this);
		optionsMenu.add(loopAndDelayOptions);
		
		menuBar.add(optionsMenu);
	}

	/**
	 * Organizes the components in the frame, adds components and then displays.
	 */
	private void addComponentsToFrameAndDisplay() {
		setJMenuBar(menuBar);
		setLayout(new GridLayout(2,1));
		add(northPanel);
		add(start);
		pack();
		setVisible(true);
	}

	/**
	 * Main that runs the whole Minesweeper program. Takes no args.
	 * @param args Ignored.
	 */
	public static void main(String[] args) {
		new Driver();
	}

	/**
	 * Handles all actions on the GUI.
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		/*
		 * If it is the start button play Minesweeper.
		 * Which Minesweeper is played depends on the current options
		 * set in the GUI. 
		 */
		if (e.getSource().equals(start)){
	
			for(int i = 0; i < loop; i ++){
				if (hasBeenPlayed)
					mb.resetBoard();
				if(hasChangedBoardType){
					mb.displayBoard();
					hasChangedBoardType = false;
				}
				if (lm.play())
					numWins++;
				else
					numLosses++;
				numGames++;
				calculatePercentageAndSetLabels();
				hasBeenPlayed = true;
			}

		}else if (e.getSource() instanceof JRadioButton){
			/*
			 * If one of the radio buttons are clicked, this is called.
			 * The Radio Buttons change which Minesweeper is run.
			 */
			if (e.getSource().equals(winMine)){
			if(!(mb instanceof WinMineBoard) ){
				mb = new WinMineBoard();
			}
			}else{
				if(!(mb instanceof GameBoard)){
				mb = new GameBoard(rows,columns,bomb);
				}
			}
			lm.setBoard(mb);
			hasChangedBoardType = true;
			hasBeenPlayed = false;

		}else if (e.getSource() instanceof JMenuItem){
			if(e.getSource().equals(waltersDefaults)){
				new WalterOptions();
			}else{
				new LoopOptions();
			}
		}
	}

	
	private void calculatePercentageAndSetLabels() {
		wins.setText(numWins + "");
		loss.setText(numLosses + "");
		games.setText(numGames + "");
		DecimalFormat df = new DecimalFormat("0.##");
		winPercentage.setText(df.format(((double) numWins )/numGames * 100));
	}


	/**
	 * Walters GUI options.
	 *
	 */
	private class WalterOptions extends JDialog implements ActionListener{
		JLabel bombLabel, rowLabel, colLabel;
		JTextField bomb, row, col;
		JButton ok, cancel;
		
		public WalterOptions(){
			initializeComponents();
			addToFrame();
			pack();
			setVisible(true);
		}
		private void initializeComponents(){
			setDefaultCloseOperation(DISPOSE_ON_CLOSE);
			
			bombLabel = new JLabel("Number of Bombs");
			bomb = new JTextField(Driver.this.bomb + "");
			
			rowLabel = new JLabel("Number of Rows");
			row = new JTextField(Driver.this.rows + "");
			
			colLabel = new JLabel("Number of Columns");
			col = new JTextField(Driver.this.columns + "");
			
			ok = new JButton("OK");
			ok.addActionListener(this);
			
			cancel = new JButton("Cancel");
			cancel.addActionListener(this);
		}
		private void addToFrame() {
			setLayout(new GridLayout(4,2));
			add(bombLabel);
			add(bomb);
			add(rowLabel);
			add(row);
			add(colLabel);
			add(col);
			add(ok);
			add(cancel);
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			if (e.getSource().equals(ok)){
				int tempBomb;
				int tempRows;
				int tempCols;
				try{
					tempBomb = Integer.parseInt(bomb.getText());
					tempRows = Integer.parseInt(row.getText());
					tempCols = Integer.parseInt(col.getText());
				}catch (NumberFormatException nfe){
					JOptionPane.showMessageDialog(this, "Please enter only numbers");
					return;
				}
				Driver.this.bomb = tempBomb;
				Driver.this.rows = tempRows;
				Driver.this.columns = tempCols;
				mb = new GameBoard(Driver.this.rows, Driver.this.columns, Driver.this.bomb);
				lm.setBoard(mb);
			}else if(e.getSource().equals(cancel)){
			}
			dispose();
		}
	}
	
	/**
	 * Loop and delay options.
	 *
	 */
	private class LoopOptions extends JDialog implements ActionListener{
		JLabel loopLabel;
		JTextField loop;
		JButton ok, cancel;
		
		public LoopOptions(){
			initializeComponents();
			addToFrame();
			pack();
			setVisible(true);
		}
		private void initializeComponents(){
			setDefaultCloseOperation(DISPOSE_ON_CLOSE);
			
			loopLabel = new JLabel("Number of times to loop");
			loop = new JTextField(Driver.this.loop + "");
			
			ok = new JButton("OK");
			ok.addActionListener(this);
			
			cancel = new JButton("Cancel");
			cancel.addActionListener(this);
		}
		private void addToFrame() {
			setLayout(new GridLayout(2,2));
			add(loopLabel);
			add(loop);

			add(ok);
			add(cancel);
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			if (e.getSource().equals(ok)){
				int tempLoop;
				try{
					tempLoop = Integer.parseInt(loop.getText());
				}catch (NumberFormatException nfe){
					JOptionPane.showMessageDialog(this, "Please enter only numbers");
					return;
				}
				Driver.this.loop = tempLoop;
			}else if(e.getSource().equals(cancel)){
			}
			dispose();
		}
	}
}
